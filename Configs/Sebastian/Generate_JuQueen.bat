set configurations=64_6_0_16x16x16_1x1x1 64_26_0_16x16x16_1x1x1 64_6_0_8x8x8_2x2x2 64_26_0_8x8x8_2x2x2 64_6_1_8x8x8_2x2x2 64_26_1_8x8x8_2x2x2 64_6_0_4x4x4_4x4x4 64_26_0_4x4x4_4x4x4 64_6_1_4x4x4_4x4x4 64_26_1_4x4x4_4x4x4

for %%c in (%configurations%) do (
	java.exe -cp "C:\Eclipse\plugins\org.scala-lang.scala-library_2.11.2.v20140721-095018-73fb460c1c.jar;C:\Eclipse\plugins\org.scala-lang.scala-reflect_2.11.2.v20140721-095018-73fb460c1c.jar;.\Compiler\bin;.\CompilerMacros\CompilerMacros\bin;.\Compiler\lib\*" Main .\Configs\Sebastian\JuQueen_%%c_Settings.txt .\Configs\Sebastian\JuQueen_%%c_Knowledge.txt
)