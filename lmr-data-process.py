"""
PDF Table Extraction Process for Liquor Market Review Reports
============================================================

This script extracts the Beer Sales (Net $) table from PDF reports in the lmr-reports folder
and converts it to CSV format, excluding Summary rows.

Updated to use PyPDF2 for better Python 3.12 compatibility.
"""

import os
import sys
from pathlib import Path
import re
import csv

# Try importing required packages with better error handling
PDF_LIBRARY = None
try:
    import PyPDF2
    PDF_LIBRARY = "pypdf2"
    print("Using PyPDF2 for PDF processing")
except ImportError:
    try:
        # Try pdfplumber with error catching
        import pdfplumber
        PDF_LIBRARY = "pdfplumber"
        print("Using pdfplumber for PDF processing")
    except Exception as e:
        print(f"pdfplumber import failed: {e}")
        try:
            import tabula
            PDF_LIBRARY = "tabula"
            print("Using tabula for PDF processing")
        except ImportError:
            print("No compatible PDF parsing library found.")
            print("Trying to install PyPDF2...")
            try:
                import subprocess
                subprocess.check_call([sys.executable, "-m", "pip", "install", "PyPDF2"])
                import PyPDF2
                PDF_LIBRARY = "pypdf2"
                print("Successfully installed and imported PyPDF2")
            except Exception as install_error:
                print(f"Installation failed: {install_error}")
                print("Please manually install: pip install PyPDF2")
                sys.exit(1)

# Try importing data libraries
DATA_LIBRARY = None
try:
    import polars as pl
    DATA_LIBRARY = "polars"
    print("Using Polars for data processing")
except ImportError:
    try:
        import pandas as pd
        DATA_LIBRARY = "pandas"
        print("Using Pandas for data processing")
    except ImportError:
        print("No data manipulation library found. Using basic CSV writing.")
        DATA_LIBRARY = "csv"


def find_pdf_files(folder_path="lmr-reports"):
    """Find all PDF files in the specified folder."""
    folder = Path(folder_path)
    if not folder.exists():
        raise FileNotFoundError(f"Folder {folder_path} not found")
    
    pdf_files = list(folder.glob("*.pdf"))
    if not pdf_files:
        raise FileNotFoundError(f"No PDF files found in {folder_path}")
    
    return pdf_files


def extract_tables_pdfplumber(pdf_path):
    """Extract tables using pdfplumber library."""
    tables = []
    
    with pdfplumber.open(pdf_path) as pdf:
        for page_num, page in enumerate(pdf.pages, 1):
            print(f"Processing page {page_num}...")
            
            # Extract tables from the page
            page_tables = page.extract_tables()
            
            for table_num, table in enumerate(page_tables):
                if table and len(table) > 1:  # Ensure table has data
                    # Look for tables containing "Beer Sales" in headers
                    header_row = table[0] if table else []
                    header_text = " ".join(str(cell) for cell in header_row if cell)
                    
                    if "beer" in header_text.lower() and ("sales" in header_text.lower() or "net" in header_text.lower()):
                        print(f"Found Beer Sales table on page {page_num}, table {table_num + 1}")
                        tables.append({
                            'page': page_num,
                            'table_num': table_num + 1,
                            'data': table
                        })
    
    return tables


def extract_tables_camelot(pdf_path):
    """Extract tables using camelot library."""
    try:
        # Extract all tables from PDF
        tables_raw = camelot.read_pdf(str(pdf_path), pages='all', flavor='lattice')
        tables = []
        
        for i, table in enumerate(tables_raw):
            df = table.df
            if not df.empty:
                # Convert to list format similar to pdfplumber
                table_data = [df.columns.tolist()] + df.values.tolist()
                
                # Check if this contains beer sales data
                header_text = " ".join(str(cell) for row in table_data[:3] for cell in row if cell)
                
                if "beer" in header_text.lower() and ("sales" in header_text.lower() or "net" in header_text.lower()):
                    print(f"Found Beer Sales table {i + 1}")
                    tables.append({
                        'page': table.page,
                        'table_num': i + 1,
                        'data': table_data
                    })
        
        return tables
    except Exception as e:
        print(f"Error with camelot extraction: {e}")
        return []


def clean_beer_sales_data(table_data):
    """Clean and process the beer sales table data."""
    if not table_data:
        return []
    
    cleaned_rows = []
    
    for row_idx, row in enumerate(table_data):
        # Skip empty rows
        if not any(str(cell).strip() for cell in row if cell is not None):
            continue
            
        # Clean each cell
        cleaned_row = []
        for cell in row:
            if cell is None:
                cleaned_cell = ""
            else:
                # Convert to string and clean
                cleaned_cell = str(cell).strip()
                # Remove extra whitespace
                cleaned_cell = re.sub(r'\s+', ' ', cleaned_cell)
            cleaned_row.append(cleaned_cell)
        
        # Skip rows that are clearly summary rows
        row_text = " ".join(cleaned_row).lower()
        if any(keyword in row_text for keyword in ['summary', 'total', 'subtotal', 'grand total']):
            print(f"Skipping summary row: {cleaned_row}")
            continue
            
        cleaned_rows.append(cleaned_row)
    
    return cleaned_rows


def save_to_csv_polars(data, output_path):
    """Save data to CSV using polars."""
    if not data:
        print("No data to save")
        return
    
    # Create DataFrame
    if len(data) > 1:
        headers = data[0]
        rows = data[1:]
        df = pl.DataFrame(rows, schema=headers, orient="row")
    else:
        df = pl.DataFrame(data)
    
    # Save to CSV
    df.write_csv(output_path)
    print(f"Data saved to {output_path}")


def save_to_csv_pandas(data, output_path):
    """Save data to CSV using pandas."""
    if not data:
        print("No data to save")
        return
    
    # Create DataFrame
    if len(data) > 1:
        headers = data[0]
        rows = data[1:]
        df = pd.DataFrame(rows, columns=headers)
    else:
        df = pd.DataFrame(data)
    
    # Save to CSV
    df.to_csv(output_path, index=False)
    print(f"Data saved to {output_path}")


def process_pdf_file(pdf_path, output_dir="output"):
    """Process a single PDF file and extract beer sales data."""
    print(f"\nProcessing: {pdf_path.name}")
    
    # Create output directory
    Path(output_dir).mkdir(exist_ok=True)
    
    tables = []
    
    # Extract data based on available library
    if PDF_LIBRARY == "pdfplumber":
        tables = extract_tables_pdfplumber(pdf_path)
    elif PDF_LIBRARY == "pypdf2":
        # Extract text and parse for tables
        text_pages = extract_text_pypdf2(pdf_path)
        tables = parse_text_for_tables(text_pages)
    elif PDF_LIBRARY == "tabula":
        print("Tabula processing not yet implemented in this version")
        return
    else:
        print(f"Unsupported PDF library: {PDF_LIBRARY}")
        return
    
    if not tables:
        print("No Beer Sales tables found in this PDF")
        print("You may need to manually inspect the PDF structure or try a different extraction method.")
        return
    
    # Process each found table
    for table_info in tables:
        print(f"\nProcessing Beer Sales data from page {table_info['page']}")
        
        # Clean the data
        cleaned_data = clean_beer_sales_data(table_info['data'])
        
        if cleaned_data:
            # Generate output filename
            base_name = pdf_path.stem
            output_filename = f"{base_name}_beer_sales_p{table_info['page']}_t{table_info['table_num']}.csv"
            output_path = Path(output_dir) / output_filename
            
            # Save to CSV based on available library
            if DATA_LIBRARY == "polars":
                save_to_csv_polars(cleaned_data, output_path)
            elif DATA_LIBRARY == "pandas":
                save_to_csv_pandas(cleaned_data, output_path)
            else:
                save_to_csv_basic(cleaned_data, output_path)
            
            print(f"Extracted {len(cleaned_data)} data rows")
        else:
            print("No valid data found after cleaning")


def main():
    """Main processing function."""
    print("=" * 60)
    print("PDF Beer Sales Table Extraction Process")
    print("=" * 60)
    print(f"Using PDF library: {PDF_LIBRARY}")
    print(f"Using data library: {DATA_LIBRARY}")
    print()
    
    try:
        # Find PDF files
        pdf_files = find_pdf_files("lmr-reports")
        print(f"Found {len(pdf_files)} PDF file(s):")
        for pdf_file in pdf_files:
            print(f"  - {pdf_file.name}")
        print()
        
        # Process each PDF file
        for pdf_file in pdf_files:
            process_pdf_file(pdf_file)
            
        print("\n" + "=" * 60)
        print("Processing complete!")
        print("Check the 'output' folder for CSV files.")
        
    except Exception as e:
        print(f"Error: {e}")
        return 1
    
    return 0


# Additional utility functions
def preview_pdf_structure(pdf_path, max_pages=3):
    """Preview the structure of a PDF to help identify table locations."""
    print(f"\nPreviewing structure of: {pdf_path.name}")
    print("-" * 40)
    
    if PDF_LIBRARY == "pdfplumber":
        with pdfplumber.open(pdf_path) as pdf:
            for page_num, page in enumerate(pdf.pages[:max_pages], 1):
                print(f"\nPage {page_num}:")
                tables = page.extract_tables()
                print(f"  Found {len(tables)} table(s)")
                
                for i, table in enumerate(tables):
                    if table:
                        print(f"  Table {i+1}: {len(table)} rows, {len(table[0]) if table else 0} columns")
                        # Show first row as sample
                        if table and table[0]:
                            sample = " | ".join(str(cell)[:20] if cell else "" for cell in table[0][:4])
                            print(f"    Sample: {sample}...")


def interactive_extraction():
    """Interactive mode for manual table selection."""
    try:
        pdf_files = find_pdf_files("lmr-reports")
        
        if len(pdf_files) == 1:
            selected_pdf = pdf_files[0]
        else:
            print("Multiple PDF files found:")
            for i, pdf_file in enumerate(pdf_files):
                print(f"{i+1}. {pdf_file.name}")
            
            choice = input("\nSelect PDF file (number): ")
            try:
                selected_pdf = pdf_files[int(choice) - 1]
            except (ValueError, IndexError):
                print("Invalid selection")
                return
        
        # Preview structure
        preview_pdf_structure(selected_pdf)
        
        # Ask if user wants to proceed
        proceed = input("\nProceed with automatic extraction? (y/n): ")
        if proceed.lower() == 'y':
            process_pdf_file(selected_pdf)
        
    except Exception as e:
        print(f"Error in interactive mode: {e}")


if __name__ == "__main__":
    # Check if running in interactive mode
    if len(sys.argv) > 1 and sys.argv[1] == "--interactive":
        interactive_extraction()
    elif len(sys.argv) > 1 and sys.argv[1] == "--preview":
        pdf_files = find_pdf_files("lmr-reports")
        for pdf_file in pdf_files:
            preview_pdf_structure(pdf_file)
    else:
        main()