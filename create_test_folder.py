import os
import re
from bs4 import BeautifulSoup

def extract_tests_from_html(file_path, output_dir):
    with open(file_path, 'r') as file:
        soup = BeautifulSoup(file, 'html.parser')

    text = soup.get_text()

    pattern = re.compile(r"Test\s+(\d+):.*?\n(.*?)(?=(?:Test\s+\d+:)|\Z)", re.DOTALL)
    matches = pattern.findall(text)

    os.makedirs(output_dir, exist_ok=True)

    for test_num, code in matches:
        cleaned_code = code.strip()
        with open(os.path.join(output_dir, f"{test_num}.txt"), 'w') as f:
            f.write(cleaned_code)
        print(f"Wrote test {test_num}.txt")

extract_tests_from_html('part3tests.html', 'tests3')
