set configurations=2_6_0_4x4x4_1x1x1 2_26_0_4x4x4_1x1x1 2_6_0_2x2x2_2x2x2 2_26_0_2x2x2_2x2x2 2_6_1_2x2x2_2x2x2 2_26_1_2x2x2_2x2x2

for %%c in (%configurations%) do (
	java.exe -cp "C:\Eclipse\plugins\org.scala-lang.scala-library_2.11.2.v20140721-095018-73fb460c1c.jar;C:\Eclipse\plugins\org.scala-lang.scala-reflect_2.11.2.v20140721-095018-73fb460c1c.jar;.\Compiler\bin;.\CompilerMacros\CompilerMacros\bin;.\Compiler\lib\*" Main .\Configs\Sebastian\LSS_%%c_Settings.txt .\Configs\Sebastian\LSS_%%c_Knowledge.txt
)