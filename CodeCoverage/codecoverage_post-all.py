#!/usr/bin/env python3

import sys, datetime, os, shutil, subprocess, argparse
from bs4 import BeautifulSoup

def parse_dirs(filename):
    with open(filename) as file:
         dirs = [line.rstrip('\n') for line in file]
    return dirs

def move(dirstart, dirend, filename):
    shutil.move(os.path.join(dirstart, filename), os.path.join(dirend, filename))
    return

def postprocess_report(coverage_report_dir, test_dirs):
    # Modify overall coverage report to include links to individual test coverage reports
    html = os.path.join(coverage_report_dir, "CodeCoverage", "__CODE_COVERAGE.HTML")
    with open(html) as file:
        content = file.read()
        soup = BeautifulSoup(content, 'html.parser')
        file.close()

    header_tr = soup.find_all("tr")[2]
    tbody = soup.tbody
    overall_data_row = soup.find_all("tr")[4]
    overall_data_row['BGCOLOR'] = "#F0F0F0"

    newcol = soup.new_tag("th", ALIGN="left", ROWSPAN="2")
    newcol.string = "Test Name"
    header_tr.insert(1, newcol)

    all_cases_col = soup.new_tag("td", ALIGN="left")
    all_cases_col.string = "All cases"
    overall_data_row.insert(1, all_cases_col)

    for dir in test_dirs:
        test_html = os.path.join(dir, "CodeCoverage", "__CODE_COVERAGE.HTML")
        link = os.path.join(dir, "CODE_COVERAGE.HTML")
        rel_link = os.path.relpath(link, os.path.join(coverage_report_dir, "CodeCoverage/"))
        if os.path.exists(test_html) and os.path.exists(link):
            test_soup = BeautifulSoup(open(test_html).read(), 'html.parser')
            test_data_row = test_soup.find_all("tr")[4]
            test_label_col = test_soup.new_tag("td", ALIGN="left")
            link_tag = test_soup.new_tag("a", href=rel_link, target="_blank")
            name = os.path.relpath(dir, coverage_report_dir)
            link_tag.string = name
            test_label_col.append(link_tag)
            test_data_row.insert(1, test_label_col)
            tbody.append(test_data_row)

    with open(html, "w") as file:
        file.write(str(soup))
        file.close()
    return

def fileexists(file):
    if os.path.exists(file):
        return os.path.abspath(file)
    raise argparse.ArgumentTypeError("%r could not be found." % file)

parser = argparse.ArgumentParser(description='Run Code Coverage')
parser.add_argument('ref_dirs', help='file containing list of reference directories')
parser.add_argument('run_dirs', help='file containing list of run directories')
parser.add_argument('--comp', dest='comp', type=fileexists, help='file that specifies source code files to be included or excluded in code coverage report')
parser.add_argument('--outdir', dest='outdir', help='code coverage report output directory name')
parser.add_argument('--overwrite', action='store_true', dest='ovr', help='overwrite existing code coverage report')
parser.add_argument('--unitbuild', dest='unitbuild', help='directory containing .dyn files from unit test execution')
args = parser.parse_args(sys.argv[1:])

run_dirs = parse_dirs(args.run_dirs)
comp = args.comp
date = datetime.datetime.utcnow().strftime("%Y-%m-%d-%H-%M-%S-UTC")
coverage_dir = os.path.join(os.getcwd(), "CodeCoverage_" + date)
if args.outdir: coverage_dir = os.path.join(os.getcwd(), args.outdir)
if os.path.isdir(coverage_dir):
    if args.ovr:
        shutil.rmtree(coverage_dir)
    else:
        raise Exception("Code coverage report already exists in output location")
os.makedirs(coverage_dir)

dpi_list = ""
individual_coverage_dirs = []
for dir in run_dirs:
    if os.path.exists(os.path.join(dir, "CODE_COVERAGE.HTML")):
        test_coverage_dir = os.path.join(coverage_dir, os.path.dirname(os.path.dirname(os.path.relpath(dir, os.getcwd()))))
        os.makedirs(test_coverage_dir)
        individual_coverage_dirs.append(test_coverage_dir)
        move(dir, test_coverage_dir, "CODE_COVERAGE.HTML")
        move(dir, test_coverage_dir, "CodeCoverage")
        move(dir, test_coverage_dir, "pgopti.dpi")
        move(dir, test_coverage_dir, "codecoverage.xml")
        dpi_list += os.path.join(os.path.dirname(os.path.dirname(os.path.relpath(dir, os.getcwd()))), "pgopti.dpi") + " "

# use spi file from first test case, they are all the same
spi = os.path.join(run_dirs[0], "pgopti.spi")
codecov_options = "codecov -bcolor FFA07A -nopmeter -include-nonexec -showdirnames -beginblkdsbl BEGIN_EXCLUDE -endblkdsbl END_EXCLUDE -onelinedsbl NO_COVER -xmlbcvrgfull codecoverage.xml -spi " + spi
if comp:
    codecov_options += " -comp"
    codecov_options += " " + comp

# If a unit test directory is provided, run code coverage report and add to list
if args.unitbuild:
    unitdir = args.unitbuild
    subprocess.run(["profmerge"], cwd=unitdir, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    unit_coverage_dir = os.path.join(coverage_dir, "unit_tests")
    os.makedirs(unit_coverage_dir)
    individual_coverage_dirs.append(unit_coverage_dir)
    move(unitdir, unit_coverage_dir, "pgopti.dpi")
    subprocess.run(codecov_options, cwd=unit_coverage_dir, check=True, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    dpi_list += os.path.join(unit_coverage_dir, "pgopti.dpi")
	
# merge dpi files into one
subprocess.run("profmerge -a " + dpi_list, cwd=coverage_dir, check=True, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# run cumulative code coverage report
subprocess.run(codecov_options, cwd=coverage_dir, check=True, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
# post process cumulative report to link to individual reports
postprocess_report(coverage_dir, individual_coverage_dirs)

#clean up files
dpi_files = [file for file in os.listdir(coverage_dir) if "dpi" in file]
for file in dpi_files:
    os.remove(os.path.join(coverage_dir, file))
for dir in run_dirs:
    delete_files = [file for file in os.listdir(dir) if "pgopti" in file]
    for file in delete_files:
        os.remove(os.path.join(dir, file))
