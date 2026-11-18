# Read file
extract_his <- function(file_name = "IPD_HIS.xlsx", source = "ipd") {
  
  # path
  his_path <- file.path(data_path, file_name)
  
  # Read the his file
  his <- read_excel(his_path)
  
  # Select columns
  his <- his |>
    select(身份证号, 诊断)
  
  # Rename columns 
  his <- his |>
    rename(
      id_number = `身份证号`,
      diagnosis_name = `诊断`
    )
  
  # Data type by columns
  his <- his |>
    mutate(
      id_number = as.character(id_number), 
      diagnosis_name = as.character(diagnosis_name)
    )
  
  # Dx pattern: 1.耳鸣、耳聋病,特发性突聋2.双侧感音神经性听觉丧失3.耳鸣4.玫瑰糠疹5.甲状腺结节6.甲状腺功能减退症7.混合型颈椎病
  # i.e., many diagnosis, separated by `1.`, `2.` etc
  # Remove the number and dot and create a matrix, each dx a column
  # Additional Dx split, 
  # Some splitted Dx are actually multi-disease, e.g. 
  # 冠状动脉粥样硬化性心脏病,稳定型心绞痛,心功能Ⅰ级
  # 1. In column Dx Detect all Chinese punctuation and change to English `,`
  # If the split result has `,`, further split the terms separated by `,`
  his <- his |>
    mutate(
      # Extract numbered diagnoses
      Dx = str_extract_all(
        diagnosis_name,
        "\\d+\\..*?(?=\\d+\\.|$)"
      ),
      
      # Remove numbered prefixes
      Dx = map(
        Dx,
        ~ str_remove(.x, "^\\d+\\.") |>
          str_trim()
      ),
      
      # Normalize Chinese punctuation
      Dx = map(
        Dx,
        ~ str_replace_all(.x, "[，、；;]", ",")
      ),
      
      # Split multi-disease entries and remove empty terms
      Dx = map(
        Dx,
        ~ str_split(.x, ",") |>
          unlist() |>
          str_trim() |>
          str_subset(".+")
      )
    ) |>
    unnest_wider(Dx, names_sep = "")
  
  
  # The wide format Dx1 -- Dx20 to long format
  # Colname name `Dx`
  # Value name `Disease`
  his <- his |>
    pivot_longer(
      cols = matches("^Dx\\d+$"),  # Automatically select Dx1, Dx2, ..., DxN
      names_to = "Dx",             # Store original column names
      values_to = "Disease",       # Store cell values
      values_drop_na = TRUE        # Remove missing disease entries
    )
  
  # Join the column `Disease` with icd10_chn
  his <- his |>
    left_join(icd10_chn, by = c("Disease" = "疾病诊断名称"))
  
  # Remove the original dx string in column dx
  his <- his |>
    select(-diagnosis_name)
  
  # Rename the icd column
  his <- his |>
    rename(
      raw_icd = "疾病诊断编码"
    )
  
  # Create 3character ICD10 category
  his <- his |>
    mutate(icd_category = str_sub(raw_icd, 1, 3))
  
  # Add column `source` as ipd
  his <- his |>
    mutate(soure = source)
}
