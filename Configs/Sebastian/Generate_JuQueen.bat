set configurations=64_6_0_16x16x16_1x1x1 64_26_0_16x16x16_1x1x1 64_6_0_8x8x8_2x2x2 64_26_0_8x8x8_2x2x2 64_6_1_8x8x8_2x2x2 64_26_1_8x8x8_2x2x2 64_6_0_4x4x4_4x4x4 64_26_0_4x4x4_4x4x4 64_6_1_4x4x4_4x4x4 64_26_1_4x4x4_4x4x4

for %%c in (%configurations%) do (
	java.exe -cp "C:\Eclipse\configuration\org.eclipse.osgi\bundles\286\1\.cp\lib\scala-library.jar;.\Compiler\bin;.\Compiler\lib\cloning-1.9.0.jar;.\Compiler\lib\objenesis-2.1.jar" Main .\Configs\Sebastian\JuQueen_%%c_Settings.txt .\Configs\Sebastian\JuQueen_%%c_Knowledge.txt
)