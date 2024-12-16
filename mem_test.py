import subprocess
import resource

def run_ocaml_program(input_data, method, step, window_size):
    command = ['dune', 'exec', 'FP3', '--', '-m'] + [method] + ['-s', str(step), '-w', str(window_size)]
    
    resource.setrlimit(resource.RLIMIT_AS, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))

    process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    print("Running")
    _, stderr = process.communicate(input=input_data.encode())

    usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    max_memory = usage.ru_maxrss / 1024
    print(f"Maximum memory usage: {max_memory:.2f} MB")

    if stderr:
        print("Error:", stderr.decode())

if __name__ == "__main__":
    with open("mem_test_input", 'r') as file:
        input_data = file.read()
    
    method = 'lagrange'
    step = 1
    window_size = 4
    
    run_ocaml_program(input_data, method, step, window_size)
