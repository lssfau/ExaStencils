import sys,os
from pathlib import Path

p = Path('./')
#anchor = os.getcwd()
for f in p.glob('**/*'):
    
    print(f.name)
    if f.name.endswith('.exa4'):
        #os.chdir(f.base)
        print('replacing in ' + f.name)
        with open(f.name) as f:
            newText=f.read().replace('[active]', '<active>').replace('[previous]', '<previous>').replace('[next]', '<next>')

        with open(f.name, "w") as f:
            f.write(newText)
       
      