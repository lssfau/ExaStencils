import sys


def indent(i):
    return i * 4 * ' '


def generate_line(i, content):
    return f'{indent(i)}{content}\n'


def generate_script_line(content):
    return f'{indent(2)}- {content}\n'


def generate_docker_image(docker_image_name, output_file):
    content = ''
    content += generate_line(0, 'generate-docker-image:')

    content += generate_line(1, 'when: manual')

    content += generate_line(1, 'image: docker:latest')

    content += generate_line(1, 'tags:')
    content += generate_script_line('docker-docker')

    content += generate_line(1, 'script:')
    content += generate_script_line('docker login -u gitlab-ci-token -p $CI_JOB_TOKEN $CI_REGISTRY')
    content += generate_script_line(f'docker pull i10git.cs.fau.de:5005/exastencils/exastencils/{docker_image_name} || true')
    content += generate_script_line(f'docker build --pull . -f  dockerfiles/{docker_image_name}.Dockerfile -t i10git.cs.fau.de:5005/exastencils/exastencils/{docker_image_name}')
    content += generate_script_line(f'docker push i10git.cs.fau.de:5005/exastencils/exastencils/{docker_image_name}')

    content += '\n'
    output_file.write(content)


def generate_build(docker_image_name, output_file):
    content = ''
    content += generate_line(0, 'build:generator:')

    content += generate_line(1, 'stage: build')

    content += generate_line(1, f'image: i10git.cs.fau.de:5005/exastencils/exastencils/{docker_image_name}')

    content += generate_line(1, 'tags:')
    content += generate_script_line('docker')

    content += generate_line(1, 'artifacts:')
    content += generate_line(2, 'paths:')
    content += generate_line(3, '- Compiler/compiler.jar')
    content += generate_line(3, '- Examples')
    content += generate_line(3, '- Testing')
    content += generate_line(2, 'expire_in: 1 weeks')

    content += generate_line(1, 'script:')
    content += generate_script_line('java -version')
    content += generate_script_line('git clone https://i10git.cs.fau.de/software/scala.git')
    content += generate_script_line('cd Compiler')
    content += generate_script_line('ant -Dscala.dir=../scala')
    content += generate_script_line('cd ../Testing')

    content += '\n'
    output_file.write(content)


def generate_single_test(test_name, test_execution, avx, cuda, docker_image_name, output_file):
    content = ''
    content += generate_line(0, f'test:{test_name}:')

    content += generate_line(1, 'stage: test')

    content += generate_line(1, 'dependencies:')
    content += generate_line(2, '- build:generator')

    content += generate_line(1, f'image: i10git.cs.fau.de:5005/exastencils/exastencils/{docker_image_name}')

    content += generate_line(1, 'tags:')
    content += generate_script_line('docker')
    if cuda:
        content += generate_script_line('cuda')
    if avx:
        content += generate_script_line('AVX')

    content += generate_line(1, f'artifacts:')
    content += generate_line(2, f'when: on_failure')
    content += generate_line(2, f'paths:')
    content += generate_line(3, f'- Testing/output/Debug/*')
    content += generate_line(2, f'expire_in: 1 weeks\n')

    content += generate_line(1, 'script:')
    content += generate_script_line('java -version')
    content += generate_script_line('python3 --version')
    content += generate_script_line('mpirun --version')
    if cuda:
        content += generate_script_line('nvcc --version')
        content += generate_script_line('updatedb')
        content += generate_script_line('locate cuda.h')
        content += generate_script_line('locate libcudart')
        content += generate_script_line('export CPATH=$CPATH:/usr/local/cuda-10.1/targets/x86_64-linux/include')
        content += generate_script_line('ln -s /usr/local/cuda-10.1/targets/x86_64-linux/lib/libcudart.so /usr/lib')
    content += generate_script_line('cd Testing')
    content += generate_script_line(test_execution)

    content += '\n'
    output_file.write(content)


def generate_tests(generator_path, path_to_test_config, docker_image_name, output_file):
    with open(path_to_test_config, 'r') as config_file:
        for line in config_file:
            line = line.strip()
            if not line or line[0] == '#':
                continue
            str_list = line.split()

            test_execution = ''
            test_execution += f'python3 run_test.py {generator_path} {str_list[0]} {str_list[2]} "{str_list[3]}" ' \
                              f'{str_list[4]} {str_list[5]} {str_list[6]}'
            test_name = str_list[0]

            avx = False
            cuda = False
            if len(str_list) > 7:
                if str_list[7].lower() == 'avx':
                    test_execution += ' Platform/anyavx.platform'
                    avx = True
                elif str_list[7].lower() == 'avx2':
                    test_execution += ' Platform/anyavx2.platform'
                    avx = True
                elif str_list[7].lower() == 'gpu':
                    test_execution += ' Platform/chimaira-gpu.platform'
                    cuda = True
                else:
                    test_execution += ' Platform/random.platform'
            else:
                test_execution += ' Platform/random.platform'
            test_execution += f' output'

            generate_single_test(test_name, test_execution, avx, cuda, docker_image_name, output_file)


def main():
    generator_path = '../Compiler/compiler.jar'
    path_to_test_config = 'Testing/test_confs.txt'
    docker_image_name = 'ubuntu-18.04-openjdk-11'
    with open('.gitlab-ci.yml', 'w') as file:
        generate_docker_image(docker_image_name, file)
        generate_build(docker_image_name, file)
        generate_tests(generator_path, path_to_test_config, docker_image_name, file)


if __name__ == "__main__":
    main()
