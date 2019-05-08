import sys, os, subprocess


def generate_settings_file(problem_name: str, output_path: str, exa_file_names: [str]):
    tmp = f'user\t= "Guest"\n\n'
    tmp += f'basePathPrefix\t= "{output_path}"\n\n'
    for name in exa_file_names:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                tmp += f'l{i}file\t= {problem_name}.exa{i}\n'

    tmp += f'debugL1File\t= "Debug/{problem_name}_debug.exa1"\n'
    tmp += f'debugL2File\t= "Debug/{problem_name}_debug.exa2"\n'
    tmp += f'debugL3File\t= "Debug/{problem_name}_debug.exa3"\n'
    tmp += f'debugL4File\t= "Debug/{problem_name}_debug.exa4"\n\n'
    tmp += f'htmlLogFile\t= "Debug/{problem_name}_log.html"\n\n'
    tmp += f'outputPath\t= "generated/{problem_name}"\n\n'
    tmp += f'produceHtmlLog\t= true\n'
    tmp += f'timeStrategies\t= true\n\n'
    tmp += f'buildfileGenerators\t= {{"MakefileGenerator"}}\n'
    with open(f'{output_path}/{problem_name}.settings', "w") as file:
        print(tmp, file=file)


def run_test(problem_name: str, output_path: str, generator_path: str, knowledge_path: str, platform_path: str,
             exa_file_names: [str], expected_results_path: str):
    if not os.path.exists(output_path):
        os.makedirs(output_path)
    generate_settings_file(problem_name, output_path, exa_file_names)
    for name in exa_file_names:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                subprocess.run(['cp', name, f'{output_path}/{problem_name}.exa{i}'])
    result = subprocess.run(['java', '-cp', generator_path, 'Main', f'{output_path}/{problem_name}.settings',
                             knowledge_path, platform_path], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    return result.returncode


def main():
    problem_name = sys.argv[1]
    output_path = sys.argv[2]
    generator_path = sys.argv[3]
    knowledge_path = sys.argv[4]
    platform_path = sys.argv[5]
    exa_files_str = sys.argv[6]
    expected_results_path = sys.argv[7]
    if exa_files_str == '*':
        exa_files = []
    else:
        exa_files = exa_files_str.split(';')
    run_test(problem_name, output_path, generator_path, knowledge_path, platform_path, exa_files, expected_results_path)


if __name__ == "__main__":
    main()
