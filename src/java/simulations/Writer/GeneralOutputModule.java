package simulations.Writer;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import model.HoundDriveStrategyConfig;
import model.ScenarioInfo;

public class GeneralOutputModule {

    public void writeGeneralOutput(Workbook workbook, ScenarioInfo scenarioInfo, String simName, int sheetIndex) {
        String sheetName = "Allgemein";
        Sheet sheet = workbook.getSheet(sheetName);
        if (sheet != null) {
            return;
        }
        sheet = workbook.createSheet(sheetName);

        int rowIndex = 0;

        Row row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Szenario:");
        row.createCell(1).setCellValue(simName);

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Anzahl Schafe:");
        row.createCell(1).setCellValue(scenarioInfo.getTotalSheepCount());

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Anzahl Hunde:");
        row.createCell(1).setCellValue(scenarioInfo.getTotalHoundCount());

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Schafe Timeout:");
        row.createCell(1).setCellValue(scenarioInfo.getSheepWaitTime());

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Hunde Timeout:");
        row.createCell(1).setCellValue(scenarioInfo.getHoundWaitTime());

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Hunde Timeout relativ:");
        row.createCell(1).setCellValue(scenarioInfo.getHoundRelativeWaitTime());

        rowIndex++;
        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Hunde Strategie");

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Cluster swarm:");
        row.createCell(1).setCellValue(HoundDriveStrategyConfig.getClusterSwarm());

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Select swarm:");
        row.createCell(1).setCellValue(HoundDriveStrategyConfig.getSelectSwarm());

        row = sheet.createRow(rowIndex++);
        row.createCell(0).setCellValue("Drive:");
        row.createCell(1).setCellValue(HoundDriveStrategyConfig.getDrive());
    }
}