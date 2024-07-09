package simulations.Writer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import org.javatuples.Pair;

import model.HoundDriveStrategyConfig;
import model.ScenarioInfo;

public class SimulationFileWriter {
    private static GeneralOutputModule generalOutputModule = new GeneralOutputModule();
    private static SheepCaptureTimesModule sheepCaptureTimesModule = new SheepCaptureTimesModule();

    public static void writeResults(ScenarioInfo scenarioInfo, List<Pair<String, String>> sheepCapturedTimes,
            String duration) {
        String jcm = System.getProperty("simName");
        if (jcm.indexOf('.') == -1) {
            jcm += ".jcm";
        }
        String simName = jcm.substring(0, jcm.lastIndexOf('.'));

        String prefixDir = "simulations/results";
        // String fullDir = String.format("%s/%s", prefixDir, simName);
        String fullDir = String.format("%s", prefixDir);
        if (!new File(fullDir).exists()) {
            new File(fullDir).mkdir();
        }

        int houndDriveStrategyClusterSwarm = HoundDriveStrategyConfig.getClusterSwarm();
        int houndDriveStrategySelectSwarm = HoundDriveStrategyConfig.getSelectSwarm();
        int houndDriveStrategyDrive = HoundDriveStrategyConfig.getDrive();

        String houndDriveStrategy = String.format("%s%s%s", houndDriveStrategyClusterSwarm,
                houndDriveStrategySelectSwarm,
                houndDriveStrategyDrive);
        String filePath = String.format("%s/%s_%s.xlsx", fullDir, simName, houndDriveStrategy);
        Workbook workbook;

        if (new File(filePath).exists()) {
            try (FileInputStream fis = new FileInputStream(filePath)) {
                workbook = new XSSFWorkbook(fis);
            } catch (IOException e) {
                workbook = new XSSFWorkbook();
            }
        } else {
            workbook = new XSSFWorkbook();
        }

        generalOutputModule.writeGeneralOutput(workbook, scenarioInfo, simName, 0);
        sheepCaptureTimesModule.writeSheepCaptureTimes(workbook, duration, sheepCapturedTimes,
                scenarioInfo.getTotalSheepCount(), 1);

        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            workbook.write(fos);
            System.out.println("Results written to " + filePath);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (workbook != null) {
                    workbook.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}