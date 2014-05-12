set configurations=2_6_0_4x4x4_1x1x1 2_26_0_4x4x4_1x1x1 2_6_0_2x2x2_2x2x2 2_26_0_2x2x2_2x2x2 2_6_1_2x2x2_2x2x2 2_26_1_2x2x2_2x2x2

for %%c in (%configurations%) do (
	java.exe -cp "C:\Eclipse\plugins\org.scala-lang.scala-library_2.11.0.v20140415-163722-cac6383e66.jar;.\Compiler\bin;.\Compiler\lib\*" Main .\Configs\Sebastian\LSS_%%c_Settings.txt .\Configs\Sebastian\LSS_%%c_Knowledge.txt
)