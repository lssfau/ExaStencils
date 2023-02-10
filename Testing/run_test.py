import sys
import os
import subprocess


def generate_settings_file(problem_name: str, output_path: str, exa_file_names: [str]):
    tmp = f'user\t= "Guest"\n\n'

    tmp += f'basePathPrefix\t= "{output_path}"\n\n'
    for name in exa_file_names:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                tmp += f'l{i}file\t = "{problem_name}.exa{i}"\n'

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


def check_results(result_str: str, expected_results_path: str):
    EPS = 1e-6

    def print_results(generated, expected):
        
        print('Expected:\t\tGenerated:')
        for s1, s2 in zip(expected, generated):
            print(f'{s1.strip()}\t{s2.strip()}')
    with open(expected_results_path, 'r') as file:
        expected_results = file.readlines()
        expected_results = [x.strip() for x in expected_results]
        results = result_str.splitlines()
        for s1, s2 in zip(results, expected_results):
            s1 = s1.strip()
            s2 = s2.strip()
            if s1 != s2:
                try:
                    if abs(float(s1) - float(s2)) > EPS:
                        print(f'Results do not match.')
                        print(f'Expected "{s2.strip()}" but got "{s1.strip()}"')
                        print('\nFull comparison:')
                        print_results(results, expected_results)
                        return False
                except ValueError as ve:
                    print(f'Results do not match.')
                    print(f'Expected "{s2.strip()}" but got "{s1.strip()}"')
                    print('\nFull comparison:')
                    print_results(results, expected_results)
                    return False

    return True


def run_test(generator_path: str, problem_name: str, knowledge_path: str, exa_file_names: [str],
             expected_results_path: str, nprocs: int, nthreads: int, platform_path: str, output_path: str):
    if not os.path.exists(output_path):
        os.makedirs(output_path)
    generate_settings_file(problem_name, output_path, exa_file_names)
    for name in exa_file_names:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                subprocess.run(['cp', name, f'{output_path}/{problem_name}.exa{i}'])
    result = subprocess.run(['java', '-cp', generator_path, 'Main', f'{output_path}/{problem_name}.settings',
                             knowledge_path, platform_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if not result.returncode == 0:
        print(result.stderr.decode('utf-8'))
        return result.returncode
    # Fix for strange behavior of PolyExpl tests
    if "PolyExpl" in problem_name:
        problem_name += "00000"
    result = subprocess.run(['make', '-j', '-s', '-C', f'{output_path}/generated/{problem_name}'],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if not result.returncode == 0:
        print(result.stderr.decode('utf-8'))
        return result.returncode
    result = subprocess.run([f'mpirun', '--allow-run-as-root', '--oversubscribe', '--mca', 'btl_base_warn_component_unused', '0',
                             f'-np', f'{nprocs}',
                             f'{output_path}/generated/{problem_name}/exastencils'],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if not result.returncode == 0:
        print(result.stderr.decode('utf-8'))
        return result.returncode
    elif expected_results_path:
        result_str = result.stdout.decode('utf-8')
        if check_results(result_str, expected_results_path) is True:
            print(f"Test for problem \"{problem_name}\" finished successfully.")
            return result.returncode
        else:
            return -1
    else:
        print(f"Test for problem \"{problem_name}\" finished successfully.")
        return result.returncode


def main():
    generator_path = sys.argv[1]
    problem_name = sys.argv[2]
    knowledge_path = sys.argv[3]
    exa_files_str = sys.argv[4]
    expected_results_path = sys.argv[5]
    nprocs = int(sys.argv[6])
    nthreads = int(sys.argv[7])
    platform_path = sys.argv[8]
    output_path = sys.argv[9]
    if exa_files_str == '*':
        exa_files = []
    else:
        exa_files = exa_files_str.split(';')
    retval = run_test(generator_path, problem_name, knowledge_path, exa_files, expected_results_path,
                    nprocs, nthreads, platform_path, output_path)
    sys.exit(retval)


if __name__ == "__main__":
    main()
