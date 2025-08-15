# PDF Beer Sales Data Extraction Process

This document outlines the process for extracting Beer Sales (Net $) table data from PDF reports in the `lmr-reports` folder.

## Quick Start

1. **Install required packages** (choose one option):
   ```bash
   # Option 1: Full-featured (recommended)
   pip install pdfplumber polars
   
   # Option 2: Alternative PDF library
   pip install camelot-py[cv] pandas
   
   # Option 3: Minimal setup
   pip install pandas PyPDF2
   ```

2. **Run the extraction**:
   ```bash
   python lmr-data-process.py
   ```

3. **Check output**: CSV files will be saved in the `output/` folder

## Files Overview

- **`lmr-data-process.py`**: Main extraction script with full PDF parsing capabilities
- **`lmr-simple-extract.py`**: Fallback script for when PDF libraries can't be installed
- **`lmr-reports/`**: Folder containing the PDF files to process
- **`output/`**: Folder where CSV files are saved

## Detailed Process

### Step 1: PDF Detection
The script automatically finds all PDF files in the `lmr-reports` folder:
- `Liquor_Market_Review_F24_25_Q4_March_2025.pdf`

### Step 2: Table Extraction
The script searches each PDF page for tables containing "Beer Sales" data by:
1. Extracting all tables from each page
2. Examining table headers for keywords like "beer", "sales", "net"
3. Identifying the correct table structure

### Step 3: Data Cleaning
For each Beer Sales table found:
1. **Remove Summary Rows**: Automatically excludes rows containing:
   - "Summary"
   - "Total" 
   - "Subtotal"
   - "Grand Total"

2. **Clean Data**: 
   - Removes extra whitespace
   - Handles null/empty cells
   - Standardizes formatting

### Step 4: CSV Export
Cleaned data is saved as CSV with naming convention:
```
{PDF_filename}_beer_sales_p{page_number}_t{table_number}.csv
```

## Usage Options

### Automatic Processing
```bash
python lmr-data-process.py
```
Processes all PDFs automatically with default settings.

### Interactive Mode
```bash
python lmr-data-process.py --interactive
```
Allows you to:
- Select specific PDF files
- Preview table structures before extraction
- Make manual decisions about table selection

### Preview Mode
```bash
python lmr-data-process.py --preview
```
Shows the structure of tables in PDFs without extracting data.

### Simple Extraction (Fallback)
```bash
python lmr-simple-extract.py
```
Use this if PDF libraries won't install. Requires manual text extraction.

## Expected Output

For each Beer Sales table found, you'll get:
- A CSV file with clean, structured data
- Console output showing:
  - Number of pages processed
  - Tables found and their locations
  - Number of data rows extracted
  - Summary rows that were excluded

### Example Output Structure
```
output/
├── Liquor_Market_Review_F24_25_Q4_March_2025_beer_sales_p3_t1.csv
└── Liquor_Market_Review_F24_25_Q4_March_2025_beer_sales_p5_t2.csv
```

## Troubleshooting

### Package Installation Issues
If you get installation errors:

1. **Update pip**:
   ```bash
   python -m pip install --upgrade pip
   ```

2. **Try alternative packages**:
   ```bash
   # Instead of pdfplumber, try:
   pip install PyPDF2 tabula-py
   
   # Instead of polars, try:
   pip install pandas
   ```

3. **Use the simple extractor**: Run `lmr-simple-extract.py` and follow the manual text extraction instructions.

### No Tables Found
If the script reports "No Beer Sales tables found":

1. **Run preview mode** to see table structure:
   ```bash
   python lmr-data-process.py --preview
   ```

2. **Check for different keywords**: The table might be labeled differently (e.g., "Beer Revenue", "Beer Net Sales")

3. **Manual inspection**: Open the PDF and note the exact page and table location

### Data Quality Issues
If the extracted data looks wrong:

1. **Check for merged cells** in the original PDF table
2. **Look for unusual formatting** (vertical text, rotated headers)
3. **Try different PDF libraries** (pdfplumber vs camelot vs tabula)

## Customization

### Modify Search Keywords
In `lmr-data-process.py`, update the table detection logic:
```python
# Look for different table identifiers
if "beer" in header_text.lower() and ("revenue" in header_text.lower()):
```

### Change Summary Row Detection
Update the summary row exclusion rules:
```python
# Add more keywords to skip
if any(keyword in row_text for keyword in ['summary', 'total', 'subtotal', 'average']):
```

### Adjust Output Format
Modify the CSV output or add other formats (Excel, JSON):
```python
# Save as Excel instead
df.write_excel(output_path.with_suffix('.xlsx'))
```

## Data Validation

After extraction, verify the data by:
1. **Checking row counts**: Compare with the original PDF
2. **Validating numeric columns**: Ensure monetary values are properly formatted
3. **Reviewing headers**: Confirm column names are correct
4. **Spot-checking values**: Manually verify a few data points

## Next Steps

Once you have the CSV files:
1. **Data Analysis**: Load into your preferred analysis tool (R, Python, Excel)
2. **Automation**: Set up scheduled processing for new PDF reports
3. **Integration**: Connect to databases or visualization tools
4. **Quality Checks**: Implement automated data validation

## Support

If you encounter issues:
1. Check the console output for detailed error messages
2. Use `--preview` mode to understand PDF structure
3. Try the `--interactive` mode for manual control
4. Review this README for troubleshooting steps