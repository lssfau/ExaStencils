import sys,os
from pathlib import Path
from glob import iglob

#p = Path('./')
#anchor = os.getcwd()
#for f in p.glob('**/*'):

       
rootdir_glob = os.getcwd() + '/**/*' # Note the added asterisks
# This will return absolute paths
file_list = [f for f in iglob('**/*', recursive=True) if os.path.isfile(f)]
print('rootdir: ' + rootdir_glob)
for f in file_list:
    print(f)
    if f.endswith('.exa4'):
        #os.chdir(f.base)
        print('replacing in ' + f)
        with open(f) as fin:
            newText=fin.read().replace('[active]', '<active>').replace('[previous]', '<previous>').replace('[next]', '<next>')
        with open(f, "w") as fout:
            fout.write(newText)