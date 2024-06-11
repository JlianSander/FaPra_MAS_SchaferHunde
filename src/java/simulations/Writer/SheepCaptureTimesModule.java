package simulations.Writer;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import org.javatuples.Pair;

public class SheepCaptureTimesModule {

    public void writeSheepCaptureTimes(Workbook workbook, String duration,
            List<Pair<String, String>> sheepCapturedTimes, int sheepCount, int sheetIndex) {
        String sheetName = "Sheep Capture Times";
        Sheet sheet = workbook.getSheet(sheetName);
        if (sheet == null) {
            sheet = workbook.createSheet(sheetName);
        }

        int rowIndex = sheet.getLastRowNum() + 1;

        if (rowIndex == 0) {
            Row dateTimeHeaderRow = sheet.createRow(rowIndex++);
            dateTimeHeaderRow.createCell(0).setCellValue("Zeitpunkt:");

            Row durationHeaderRow = sheet.createRow(rowIndex++);
            durationHeaderRow.createCell(0).setCellValue("Laufzeit:");

            Row sheepCountHeaderRow = sheet.createRow(rowIndex++);
            sheepCountHeaderRow.createCell(0).setCellValue("Anzahl Schafe");
        } else {
            rowIndex = 1; // Skip to the second row for "Zeitpunkt"
        }

        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("dd-MM HH:mm:ss");
        String currentTime = dtf.format(now);

        // Append the current time and duration to the headers
        sheet.getRow(0).createCell(sheet.getRow(0).getLastCellNum()).setCellValue(currentTime);
        sheet.getRow(1).createCell(sheet.getRow(1).getLastCellNum()).setCellValue(duration);

        // Append the sheep data
        for (int i = 0; i < sheepCount; i++) {
            // String capturedTime = i < sheepCapturedTimes.size() ? sheepCapturedTimes.get(i).getValue1() : "-";
            String capturedTime = "-";
            if (i < sheepCapturedTimes.size()) {
                capturedTime = sheepCapturedTimes.get(i).getValue1();
                capturedTime = capturedTime.substring(0, capturedTime.lastIndexOf(":")) + ","
                        + capturedTime.substring(capturedTime.lastIndexOf(":") + 1);
            }

            Row row;
            if (sheet.getRow(3 + i) == null) {
                row = sheet.createRow(3 + i);
                row.createCell(0).setCellValue(i);
            } else {
                row = sheet.getRow(3 + i);
            }
            row.createCell(row.getLastCellNum()).setCellValue(capturedTime);
        }
    }
}