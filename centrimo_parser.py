import argparse
import os
import re
import json

def parse(file_name, outdir):

    with open(file_name, 'r') as file:
        html_content = file.read()       
    matches = re.findall('data.?=.?([\s\S]*?);', html_content)
    if len(matches) < 1:
        raise ValueError('Could not find "data" variable in CentriMo HTML file')
    
    data = json.loads(matches[0])
    
    ## meta.txt -----------------------------------------
    
    keys = ['uid', 'id', 'alt', 'len', 'motif_evalue', 'n_tested', 'total_sites', 'neg_total_sites']
    f = open(outdir + '/meta.txt', 'w')
    f.write('\t'.join(keys) + '\n')
    
    for i,motif in enumerate(data['motifs']):
       motif['uid'] = "motif_" + str(i).zfill(5)
       line = [str(motif[key]) for key in keys]
       f.write('\t'.join(line) + '\n')
    
    f.close()

    ## sites.txt ----------------------------------------
    
    max_sites = max([len(motif['sites']) for motif in data['motifs']])
        
    f = open(outdir + '/sites.txt', 'w')
    uids = [motif['uid'] for motif in data['motifs']]
    f.write('\t'.join(uids) + '\n')
    
    for i in range(max_sites):
       line = []
       
       for motif in data['motifs']:
          if i >= len(motif['sites']):
             line.append('NA')
          else:
             line.append(str(motif['sites'][i]))
       f.write('\t'.join(line) + '\n')

    f.close()
    
    ## neg_sites.txt ------------------------------------

    max_sites = max([len(motif['neg_sites']) for motif in data['motifs']])
    
    f = open(outdir + '/neg_sites.txt', 'w')
    uids = [motif['uid'] for motif in data['motifs']]
    f.write('\t'.join(uids) + '\n')
    
    for i in range(max_sites):
       line = []
       
       for motif in data['motifs']:
          if i >= len(motif['neg_sites']):
             line.append('NA')
          else:
             line.append(str(motif['neg_sites'][i]))
       f.write('\t'.join(line) + '\n')

    f.close() 
   

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Parses CentriMo HTML files')
    parser.add_argument('html_file', help='Centrimo HTML file')
    parser.add_argument('output', help='Directory for output', default="./out", nargs='?')
    args = parser.parse_args()

    file_name = args.html_file

    directory = args.output
    if not os.path.exists(directory):
       os.makedirs(directory)
    
    parse(file_name, directory)

