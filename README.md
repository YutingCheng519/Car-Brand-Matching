# Car-Brand-Matching
Data Cleaning System for Car Brands and Models
# Car Brand Extraction from Messy Text Data

## 🚗 Project Overview
This project aims to extract **brand information** from unstructured car name strings in a dataset. The original `name` field contains messy entries (e.g., abbreviations, typos, mixed formats), and our goal is to generate a clean `brand` variable through systematic text processing.

**Example Input → Output**:
Input name: "Toyota_Camry_2020_XLE" → Output brand: "Toyota"
Input name: "VW-Golf-2018" → Output brand: "Volkswagen"
Input name: "Honda_Civic_1.5T" → Output brand: "Honda"
Input name: "316i_Sedan" → Output brand: "BMW"
Input name: "Unknown_Car_Model" → Output brand: NA

---

## 📂 Data Source
- **Original Dataset**: `autos.csv`
- **Key Variable**:
  - `name`: Unstructured text containing car names (e.g., `"bmw-320i-2015"`, `"Mercedes C-Class"`)
- **Brand-Reference Data**: `brands.csv`  
  Contains official brand-model mappings for validation:
  | brand       | models               |
  |-------------|----------------------|
  | Toyota      | Camry, Corolla, ...  |
  | Volkswagen  | Golf, Passat, ...    |

---

## 🔧 Methodology

### Step 1: Text Standardization
```r
# R
clean_name <- tolower(name) %>% 
  str_replace_all("[^a-zA-Z0-9]", " ") %>%  # Remove special characters
  str_squish()                              # Trim whitespace

### Step 2: Direct Brand Matching
Priority: Match brand names directly from standardized text using regex patterns:
brand_patterns <- c(
  "toyota" = "\\b(toyota|toyo)\\b",
  "volkswagen" = "\\b(vw|volkswagen)\\b",
  "bmw" = "\\b(bmw|bmv)\\b",
  "mercedes" = "\\b(mercedes|benz|mb)\\b"
)





