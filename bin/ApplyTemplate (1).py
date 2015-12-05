# coding: utf-8

# In[63]:

import jinja2
import argparse
import csv
import os


# In[65]:

parser = argparse.ArgumentParser(description="Fill template with CSV data.")

parser.add_argument('--template', '-t', help="template file to use", required=True)
parser.add_argument('--output-dir', '-o', help="where to store filled templates", default=".")
parser.add_argument('--delimiter', '-d', help="delimiter of input csv file", default="\t")
parser.add_argument('INPUT', help="input csv file", nargs="+", default='/dev/stdin')
parser.print_help()


# In[66]:

args = parser.parse_args(["-t", "curl.template", "a.csv"])


# In[71]:

from jinja2 import Template
from jinja2 import Environment, FileSystemLoader, DictLoader

template = open(args.template).read()

env = Environment(loader=FileSystemLoader("."),
                  block_start_string="<<%",
                  block_end_string="%>>",
                  variable_start_string="{{",
                  variable_end_string="}}",
                  comment_start_string = '\#{',
                  comment_end_string = '}',
                  line_statement_prefix = '%%-',
                  line_comment_prefix = '%#',
                  trim_blocks = True,
                  autoescape = False
                  )

template = env.get_template(args.template)

for csv_file in args.INPUT:
    with open(csv_file,"r") as fh:
        reader = csv.reader(fh, delimiter=args.delimiter)
        for i,row in enumerate(reader):
            with open(os.path.join(args.output_dir, args.template+ "." + str(i) + ".test"), "w") as out_file:
                out_file.write(template.render(name="123", row=row))

