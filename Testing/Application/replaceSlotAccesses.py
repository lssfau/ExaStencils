import sys,os
from pathlib import Path
from glob import iglob


       
files = Path('.').glob('**/*')
for f in files:
    if Path.is_file(f):
    #print(f.name)
    #if f.name.endswith('exa4'):
        #os.chdir(f.base)
        print('replacing in ' + f.name)
        with open(f.name) as f:
            newText=f.read().replace('<active>', '<active>').replace('<previous>', '<previous>').replace('<next>', '<next>').replace('<nextSlot>','<nextSlot>').replace('<activeSlot>','<activeSlot>').replace('<previousSlot>','<previousSlot>')

        with open(f.name, "w") as f:
            f.write(newText)    
   