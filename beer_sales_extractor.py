#!/usr/bin/env python3
"""
Beer Sales Data Extractor for LMR Reports
==========================================

This script specifically extracts the Beer Sales (Net $) table from 
Liquor Market Review PDFs and converts it to CSV format.

The script is designed for the specific format found in the PDFs where 
the data is structured with categories and quarterly sales figures.
"""

import PyPDF2
import polars as pl
import re
from pathlib import Path


def extract_beer_sales_data(pdf_path):
    """Extract beer sales data from the PDF."""
    print(f"Processing: {pdf_path.name}")
    
    with open(pdf_path, 'rb') as file:
        pdf_reader = PyPDF2.PdfReader(file)
        
        # Extract text from page 5 (index 4) where the detailed table is
        page_text = pdf_reader.pages[4].extract_text()
        lines = page_text.split('\n')
        
        # Parse the data
        data = []
        current_category = None
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
                
            # Skip header lines
            if line in ['Beer Sales (Net $)', 'Beer', 'Net Sales $']:
                continue
                
            # Check for quarter headers
            if line.startswith('Fiscal'):
                # This is a header row with quarters
                quarters = parse_quarter_line(line)
                continue
                
            # Check for main category headers
            if line.endswith('Beer') and '$' not in line:
                current_category = line
                continue
                
            # Parse data lines with dollar amounts
            if '$' in line and current_category:
                parsed_row = parse_data_line(line, current_category)
                if parsed_row:
                    data.append(parsed_row)
                    
            # Check for summary lines (standalone dollar amounts)
            elif re.match(r'^\$[\d,]+$', line):
                # This might be a subtotal - we'll skip these as per requirements
                continue
                
        return data


def parse_quarter_line(line):
    """Parse the quarter header line to extract column names."""
    # The line contains fiscal quarters
    quarters = []
    if 'Fiscal 2023/24 Q4' in line:
        quarters.append('Fiscal 2023/24 Q4')
    if 'Fiscal 2024/25' in line:
        for q in ['Q1', 'Q2', 'Q3', 'Q4']:
            if q in line:
                quarters.append(f'Fiscal 2024/25 {q}')
    return quarters


def parse_data_line(line, category):
    """Parse a line containing beer sales data."""
    # Remove category prefix if it exists
    if category in line:
        line = line.replace(category, '').strip()
    
    # Extract product name and dollar amounts
    parts = re.findall(r'[^$\d,]+|[\$\d,]+', line)
    
    product_name = ""
    amounts = []
    
    for part in parts:
        part = part.strip()
        if part.startswith('$'):
            # Clean and convert dollar amount
            clean_amount = part.replace('$', '').replace(',', '')
            amounts.append(clean_amount)
        elif part and not part.isspace():
            if not product_name:
                product_name = part.strip()
    
    if product_name and amounts:
        # Create row with category, product, and amounts
        row = [category, product_name] + amounts
        return row
    
    return None


def create_beer_sales_csv(pdf_path, output_dir="output"):
    """Create CSV file from beer sales data."""
    
    # Create output directory
    Path(output_dir).mkdir(exist_ok=True)
    
    # Extract the data
    data = extract_beer_sales_data(pdf_path)
    
    if not data:
        print("No beer sales data found")
        return
    
    # Create headers
    headers = [
        'Category', 
        'Product', 
        'Fiscal 2023/24 Q4', 
        'Fiscal 2024/25 Q1', 
        'Fiscal 2024/25 Q2', 
        'Fiscal 2024/25 Q3', 
        'Fiscal 2024/25 Q4'
    ]
    
    # Filter out rows that are summaries (looking for total/summary keywords)
    filtered_data = []
    for row in data:
        if len(row) >= 2:
            product_name = row[1].lower()
            category_name = row[0].lower()
            
            # Skip summary rows
            if any(keyword in product_name or keyword in category_name 
                   for keyword in ['total', 'summary', 'subtotal', 'grand total']):
                print(f"Skipping summary row: {row}")
                continue
            
            # Ensure row has the right number of columns
            while len(row) < len(headers):
                row.append('')
            
            filtered_data.append(row[:len(headers)])  # Trim if too long
    
    if not filtered_data:
        print("No data remaining after filtering")
        return
    
    # Create DataFrame and save
    df = pl.DataFrame(filtered_data, schema=headers, orient="row")
    
    # Generate output filename
    base_name = pdf_path.stem
    output_filename = f"{base_name}_beer_sales_clean.csv"
    output_path = Path(output_dir) / output_filename
    
    df.write_csv(output_path)
    
    print(f"Successfully extracted {len(filtered_data)} rows of beer sales data")
    print(f"Data saved to: {output_path}")
    
    # Show preview
    print("\nPreview of extracted data:")
    print(df.head(10))
    
    return output_path


def main():
    """Main function to process PDFs in the lmr-reports folder."""
    
    # Find PDF files
    folder = Path("lmr-reports")
    if not folder.exists():
        print(f"Error: Folder '{folder}' not found")
        return
    
    pdf_files = list(folder.glob("*.pdf"))
    if not pdf_files:
        print(f"No PDF files found in '{folder}'")
        return
    
    print(f"Found {len(pdf_files)} PDF file(s)")
    
    # Process each PDF
    for pdf_path in pdf_files:
        try:
            create_beer_sales_csv(pdf_path)
            print("-" * 60)
        except Exception as e:
            print(f"Error processing {pdf_path.name}: {e}")


if __name__ == "__main__":
    main()