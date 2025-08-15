#!/usr/bin/env python3
"""
LMR Beer Sales Data Processor
============================

A robust processor for extracting Beer Sales (Net $) data from 
Liquor Market Review PDF reports and converting to CSV format.

This processor:
1. Automatically finds PDF files in the lmr-reports folder
2. Extracts Beer Sales data using text parsing
3. Excludes summary/total rows as requested
4. Saves clean data to CSV files
5. Handles the specific structure of LMR reports

Usage:
    python lmr_beer_sales_processor.py

Requirements:
    - PyPDF2
    - polars (or pandas)
"""

import PyPDF2
import polars as pl
import re
from pathlib import Path
import sys


def find_beer_sales_page(pdf_reader):
    """Find the page containing detailed beer sales data."""
    for page_idx, page in enumerate(pdf_reader.pages):
        text = page.extract_text()
        
        # Look for the detailed table (page that has both beer categories and dollar amounts)
        if ('Domestic - BC Commercial Beer' in text and 
            'Domestic - Other Province' in text and 
            'Import Beer' in text and
            '$' in text):
            return page_idx
    
    return None


def extract_beer_data_lines(text):
    """Extract lines containing beer sales data from page text."""
    lines = text.split('\\n')
    data_lines = []
    
    # Patterns that indicate data lines
    beer_product_patterns = [
        r'Domestic - BC Commercial Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Domestic - BC Micro Brew Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Domestic - BC Regional Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Domestic - Other Province Commercial Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Domestic - Other Province Micro Brew Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Domestic - Other Province Regional Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Asia And South Pacific Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Europe Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Mexico And Caribbean Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'Other Country Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+',
        r'USA Beer.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+.*?\$[\d,]+'
    ]
    
    for line in lines:
        line = line.strip()
        
        # Check each pattern
        for pattern in beer_product_patterns:
            if re.search(pattern, line):
                data_lines.append(line)
                break
    
    return data_lines


def parse_beer_data_line(line):
    """Parse a single beer data line into structured data."""
    
    # Extract dollar amounts
    amounts = re.findall(r'\\$([\\d,]+)', line)
    amounts = [amt.replace(',', '') for amt in amounts]
    
    # Determine category and product
    if 'Domestic - BC' in line:
        category = 'Domestic - BC Beer'
        if 'Commercial' in line:
            product = 'Commercial Beer'
        elif 'Micro Brew' in line:
            product = 'Micro Brew Beer'
        elif 'Regional' in line:
            product = 'Regional Beer'
        else:
            product = 'Unknown BC Product'
            
    elif 'Domestic - Other Province' in line:
        category = 'Domestic - Other Province Beer'
        if 'Commercial' in line:
            product = 'Commercial Beer'
        elif 'Micro Brew' in line:
            product = 'Micro Brew Beer'
        elif 'Regional' in line:
            product = 'Regional Beer'
        else:
            product = 'Unknown Other Province Product'
            
    elif any(region in line for region in ['Asia And South Pacific', 'Europe', 'Mexico And Caribbean', 'Other Country', 'USA']):
        category = 'Import Beer'
        if 'Asia And South Pacific' in line:
            product = 'Asia And South Pacific Beer'
        elif 'Europe' in line:
            product = 'Europe Beer'
        elif 'Mexico And Caribbean' in line:
            product = 'Mexico And Caribbean Beer'
        elif 'Other Country' in line:
            product = 'Other Country Beer'
        elif 'USA' in line:
            product = 'USA Beer'
        else:
            product = 'Unknown Import Product'
    else:
        category = 'Unknown Category'
        product = 'Unknown Product'
    
    if len(amounts) >= 5:  # We expect 5 quarters of data
        return [category, product] + amounts[:5]
    
    return None


def extract_beer_sales_from_pdf(pdf_path):
    """Extract beer sales data from PDF file."""
    print(f"Processing: {pdf_path.name}")
    
    with open(pdf_path, 'rb') as file:
        pdf_reader = PyPDF2.PdfReader(file)
        
        # Find the page with detailed beer sales data
        beer_page_idx = find_beer_sales_page(pdf_reader)
        
        if beer_page_idx is None:
            print("Could not find beer sales data page")
            return None
            
        print(f"Found beer sales data on page {beer_page_idx + 1}")
        
        # Extract text from the page
        page_text = pdf_reader.pages[beer_page_idx].extract_text()
        
        # Extract data lines
        data_lines = extract_beer_data_lines(page_text)
        
        if not data_lines:
            print("No data lines found")
            return None
        
        print(f"Found {len(data_lines)} potential data lines")
        
        # Parse each line
        parsed_data = []
        for line in data_lines:
            parsed_row = parse_beer_data_line(line)
            if parsed_row:
                parsed_data.append(parsed_row)
        
        return parsed_data


def save_beer_sales_csv(data, pdf_path, output_dir="output"):
    """Save beer sales data to CSV file."""
    
    if not data:
        print("No data to save")
        return None
    
    # Create headers
    headers = [
        'Category', 
        'Product', 
        'Fiscal_2023_24_Q4', 
        'Fiscal_2024_25_Q1', 
        'Fiscal_2024_25_Q2', 
        'Fiscal_2024_25_Q3', 
        'Fiscal_2024_25_Q4'
    ]
    
    # Filter out summary rows (though there shouldn't be any with our parsing method)
    filtered_data = []
    for row in data:
        if len(row) >= 2:
            product_name = row[1].lower()
            category_name = row[0].lower()
            
            # Skip any rows that might be summaries
            if any(keyword in product_name or keyword in category_name 
                   for keyword in ['total', 'summary', 'subtotal', 'grand']):
                print(f"Skipping summary row: {row}")
                continue
            
            filtered_data.append(row)
    
    if not filtered_data:
        print("No data remaining after filtering")
        return None
    
    # Create output directory
    Path(output_dir).mkdir(exist_ok=True)
    
    # Create DataFrame
    df = pl.DataFrame(filtered_data, schema=headers, orient="row")
    
    # Generate output filename
    base_name = pdf_path.stem
    output_filename = f"{base_name}_beer_sales_extracted.csv"
    output_path = Path(output_dir) / output_filename
    
    # Save to CSV
    df.write_csv(output_path)
    
    print(f"Successfully saved {len(filtered_data)} rows to: {output_path}")
    
    return output_path


def process_lmr_pdfs():
    """Process all PDF files in the lmr-reports folder."""
    
    # Find the reports folder
    reports_folder = Path("lmr-reports")
    
    if not reports_folder.exists():
        print(f"Error: Folder '{reports_folder}' not found")
        print("Please ensure the 'lmr-reports' folder exists and contains PDF files")
        return
    
    # Find PDF files
    pdf_files = list(reports_folder.glob("*.pdf"))
    
    if not pdf_files:
        print(f"No PDF files found in '{reports_folder}'")
        return
    
    print(f"Found {len(pdf_files)} PDF file(s) to process:")
    for pdf_file in pdf_files:
        print(f"  - {pdf_file.name}")
    
    print("\\n" + "=" * 60)
    
    # Process each PDF
    success_count = 0
    for pdf_path in pdf_files:
        try:
            print(f"\\nProcessing: {pdf_path.name}")
            print("-" * 40)
            
            # Extract data
            beer_data = extract_beer_sales_from_pdf(pdf_path)
            
            if beer_data:
                # Save to CSV
                output_path = save_beer_sales_csv(beer_data, pdf_path)
                if output_path:
                    success_count += 1
                    
                    # Show preview
                    if len(beer_data) <= 15:  # Show all if small dataset
                        df = pl.read_csv(output_path)
                        print("\\nExtracted data preview:")
                        print(df)
                    else:
                        df = pl.read_csv(output_path)
                        print(f"\\nExtracted data preview (first 10 of {len(beer_data)} rows):")
                        print(df.head(10))
            else:
                print("Failed to extract data from this PDF")
                
        except Exception as e:
            print(f"Error processing {pdf_path.name}: {e}")
    
    print("\\n" + "=" * 60)
    print(f"Processing complete!")
    print(f"Successfully processed {success_count} of {len(pdf_files)} PDF files")
    
    if success_count > 0:
        print("\\nOutput files can be found in the 'output' folder")
    else:
        print("\\nNo files were successfully processed")


def main():
    """Main function."""
    print("LMR Beer Sales Data Processor")
    print("=" * 40)
    
    try:
        process_lmr_pdfs()
    except KeyboardInterrupt:
        print("\\nOperation cancelled by user")
    except Exception as e:
        print(f"\\nUnexpected error: {e}")
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())