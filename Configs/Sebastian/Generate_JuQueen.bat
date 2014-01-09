set configurations=64_6_1_16x16x16_1x1x1 64_26_1_16x16x16_1x1x1 64_6_0_8x8x8_2x2x2 64_26_0_8x8x8_2x2x2 64_6_1_8x8x8_2x2x2 64_26_1_8x8x8_2x2x2

for %%c in (%configurations%) do (
	java.exe -cp "C:\Eclipse\configuration\org.eclipse.osgi\bundles\286\1\.cp\lib\scala-library.jar;.\Compiler\bin" Main .\Configs\Sebastian\JuQueen_%%c_Settings.txt .\Configs\Sebastian\JuQueen_%%c_Knowledge.txt
)