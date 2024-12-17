import subprocess
import resource
from tqdm import tqdm
from pathlib import Path


def avg_memory_usage(
    input_data: str, method: str, step: float, window_size: int
) -> float:
    sum_memory = 0
    for _ in tqdm(range(10)):
        command = (
            ["dune", "exec", "FP3", "--", "-m"]
            + [method]
            + ["-s", str(step), "-w", str(window_size)]
        )
        resource.setrlimit(
            resource.RLIMIT_AS, (resource.RLIM_INFINITY, resource.RLIM_INFINITY)
        )
        process = subprocess.Popen(
            command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        _, stderr = process.communicate(input=input_data.encode())

        usage = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
        sum_memory += usage / 1024

        if stderr:
            print("Error:", stderr.decode())

    return sum_memory / 10


with open("mem_test_input", "r") as file:
    data_100mb = file.read()
    
with open("small_test_input", "r") as file:
    data_1mb = file.read()

method = "lagrange"
step = 1
window_size = 4

avg_memory_usage_100mb = avg_memory_usage(data_100mb, method, step, window_size)
avg_memory_usage_1mb = avg_memory_usage(data_1mb, method, step, window_size)
print(avg_memory_usage_100mb - avg_memory_usage_1mb)
