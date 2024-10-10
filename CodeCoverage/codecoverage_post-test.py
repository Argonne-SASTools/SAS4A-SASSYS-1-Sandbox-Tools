#!/usr/bin/env python3

import sys, subprocess, os, argparse

def fileexists(file):
    if os.path.exists(file):
        return os.path.abspath(file)
    raise argparse.ArgumentTypeError("%r could not be found." % file)

parser = argparse.ArgumentParser(description='Run Code Coverage')
parser.add_argument('ref', help='reference directory')
parser.add_argument('run', help='run directory')
parser.add_argument('status') # this isn't used by this script
parser.add_argument('--comp', dest='comp', type=fileexists, help='file that specifies source code files to be included or excluded in code coverage report')
args = parser.parse_args(sys.argv[1:])

run_dir = args.run
comp = args.comp
prj_name = os.path.relpath(os.path.dirname(os.path.dirname(os.path.dirname(run_dir))), os.getcwd())
subprocess.run(["profmerge"], cwd=run_dir, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

codecov_options = ["codecov", "-bcolor", "FFA07A", "-nopmeter", "-include-nonexec", "-showdirnames", "-prj", prj_name, "-xmlbcvrgfull", "codecoverage.xml",
                   "-beginblkdsbl", "BEGIN_EXCLUDE", "-endblkdsbl", "END_EXCLUDE", "-onelinedsbl", "NO_COVER"]
if comp:
    codecov_options.append("-comp")
    codecov_options.append(comp)

subprocess.run(codecov_options, cwd=run_dir, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
dynfile = [file for file in os.listdir(run_dir) if ".dyn" in file][0]
os.remove(os.path.join(run_dir, dynfile))
