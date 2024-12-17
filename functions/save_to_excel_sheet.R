# This function saves multiple datasets into an Excel file, each in a separate sheet. If there's an error while writing to a sheet, it will display an error message but continue with the next sheet.
# Arguments:
#   file_path: The path where the Excel file will be saved.
#   sheets_and_data: A named list where each name is the sheet name and the value is the dataset to be written to that sheet.
save_to_excel <- function(file_path, sheets_and_data) {
  workbook <- createWorkbook()
  for (sheet_name in names(sheets_and_data)) {
    tryCatch({
      addWorksheet(workbook, sheet_name) 
      writeData(workbook, sheet = sheet_name, sheets_and_data[[sheet_name]])
    }, error = function(e) {
      message(paste("Failed to write data to sheet", sheet_name, ":", e$message))
    })
  }
  tryCatch({
    saveWorkbook(workbook, file = file_path, overwrite = TRUE)
    message("Workbook saved successfully at: ", file_path)
  }, error = function(e) {
    message("Failed to save workbook: ", e$message)
  })
}
