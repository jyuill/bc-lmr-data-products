"""
Simple PDF Text Extraction for Beer Sales Data
===============================================

This is a fallback approach that uses built-in libraries to extract text from PDF
and then parse it to find table-like structures. Use this if you can't install
the specialized PDF libraries.

"""

import re
import csv
from pathlib import Path

def extract_text_simple(pdf_path):
    """
    Placeholder for simple PDF text extraction.
    
    In practice, you would need to:
    1. Install a PDF library: pip install PyPDF2 or pdfplumber
    2. Or use an online service to convert PDF to text
    3. Or manually copy the table text from the PDF
    """
    
    print(f"To extract text from {pdf_path}, you can:")
    print("1. Install a PDF library:")
    print("   pip install pdfplumber")
    print("2. Or manually copy the Beer Sales table text to a file")
    print("3. Or use an online PDF-to-text converter")
    
    # Check if there's already extracted text
    text_file = pdf_path.with_suffix('.txt')
    if text_file.exists():
        print(f"Found text file: {text_file}")
        with open(text_file, 'r', encoding='utf-8') as f:
            return f.read()
    else:
        print(f"Create a text file: {text_file}")
        print("Copy the Beer Sales table content into it, then run this script again.")
        return None

def parse_table_text(text):
    """Parse text that contains table-like data."""
    lines = text.split('\n')
    
    # Find lines that might contain beer sales data
    beer_sales_lines = []
    in_beer_section = False
    
    for line in lines:
        line = line.strip()
        if not line:
            continue
            
        # Look for beer sales section
        if 'beer sales' in line.lower() or 'beer' in line.lower() and 'net' in line.lower():
            in_beer_section = True
            beer_sales_lines.append(line)
            continue
            
        # Stop if we hit another section
        if in_beer_section and any(keyword in line.lower() for keyword in ['wine', 'spirits', 'total', 'summary']):
            if 'summary' in line.lower():
                break  # Skip summary rows
            if line.lower().startswith('total') or line.lower().startswith('grand'):
                break  # Skip total rows
                
        if in_beer_section:
            beer_sales_lines.append(line)
    
    return beer_sales_lines

def parse_line_to_columns(line):
    """Parse a line into columns, handling various separators."""
    # Try to split by multiple spaces, tabs, or other separators
    
    # First try splitting by tabs
    if '\t' in line:
        columns = [col.strip() for col in line.split('\t')]
        return [col for col in columns if col]
    
    # Try splitting by multiple spaces (2 or more)
    columns = re.split(r'\s{2,}', line)
    columns = [col.strip() for col in columns if col.strip()]
    
    if len(columns) > 1:
        return columns
    
    # Try splitting by pipe or other separators
    for separator in ['|', ',']:
        if separator in line:
            columns = [col.strip() for col in line.split(separator)]
            return [col for col in columns if col]
    
    # If no clear separator, return as single column
    return [line.strip()]

def clean_and_validate_data(raw_data):
    """Clean the extracted data and validate it looks like a table."""
    if not raw_data:
        return []
    
    table_data = []
    
    for line in raw_data:
        if not line.strip():
            continue
            
        # Skip obvious non-data lines
        if any(keyword in line.lower() for keyword in ['summary', 'page', 'report']):
            continue
            
        columns = parse_line_to_columns(line)
        if columns:
            table_data.append(columns)
    
    # Try to identify header row and standardize column count
    if table_data:
        # Find the most common column count
        col_counts = {}
        for row in table_data:
            count = len(row)
            col_counts[count] = col_counts.get(count, 0) + 1
        
        most_common_cols = max(col_counts.keys(), key=lambda k: col_counts[k])
        
        # Filter rows to match most common column count
        filtered_data = []
        for row in table_data:
            if len(row) == most_common_cols:
                filtered_data.append(row)
            elif len(row) > most_common_cols:
                # Truncate if too many columns
                filtered_data.append(row[:most_common_cols])
            # Skip rows with too few columns
        
        return filtered_data
    
    return []

def save_to_csv_simple(data, output_path):
    """Save data to CSV using built-in csv module."""
    if not data:
        print("No data to save")
        return
    
    with open(output_path, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(data)
    
    print(f"Data saved to {output_path}")
    print(f"Saved {len(data)} rows with {len(data[0]) if data else 0} columns")

def process_simple():
    """Simple processing workflow."""
    print("Simple PDF Processing for Beer Sales Data")
    print("=" * 50)
    
    # Find PDF files
    pdf_folder = Path("lmr-reports")
    if not pdf_folder.exists():
        print(f"Folder {pdf_folder} not found")
        return
    
    pdf_files = list(pdf_folder.glob("*.pdf"))
    if not pdf_files:
        print(f"No PDF files found in {pdf_folder}")
        return
    
    # Process each PDF
    for pdf_path in pdf_files:
        print(f"\nProcessing: {pdf_path.name}")
        
        # Try to extract text
        text_content = extract_text_simple(pdf_path)
        
        if text_content:
            # Parse the text for table data
            table_lines = parse_table_text(text_content)
            
            if table_lines:
                print(f"Found {len(table_lines)} potential table lines")
                
                # Clean and structure the data
                table_data = clean_and_validate_data(table_lines)
                
                if table_data:
                    # Save to CSV
                    output_path = Path("output") / f"{pdf_path.stem}_beer_sales_simple.csv"
                    output_path.parent.mkdir(exist_ok=True)
                    
                    save_to_csv_simple(table_data, output_path)
                    
                    # Show preview
                    print("\nData preview:")
                    for i, row in enumerate(table_data[:5]):  # Show first 5 rows
                        print(f"Row {i+1}: {row}")
                    
                    if len(table_data) > 5:
                        print(f"... and {len(table_data) - 5} more rows")
                        
                else:
                    print("Could not parse table structure from text")
            else:
                print("No beer sales data found in text")
        else:
            print("No text extracted. See instructions above.")

if __name__ == "__main__":
    process_simple()