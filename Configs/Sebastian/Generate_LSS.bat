set configurations=2_6_0_4x4x4_1x1x1 2_26_0_4x4x4_1x1x1 2_6_0_2x2x2_2x2x2 2_26_0_2x2x2_2x2x2 2_6_1_2x2x2_2x2x2 2_26_1_2x2x2_2x2x2

for %%c in (%configurations%) do (
	java.exe -cp "C:\Eclipse\configuration\org.eclipse.osgi\bundles\286\1\.cp\lib\scala-library.jar;.\Compiler\bin;.\Compiler\lib\cloning-1.9.0.jar;.\Compiler\lib\objenesis-2.1.jar" Main .\Configs\Sebastian\LSS_%%c_Settings.txt .\Configs\Sebastian\LSS_%%c_Knowledge.txt
)