## bk

Bk is a tool for simple finance management.
Run bk without any options to see usage.

## Build and install

1. make
2. make install
3. add /usr/local/lib/bk to the $ERL_LIBS

## Import from Excel

For each worksheet:
1. Ctrl+` to show formulas.
2. Save as Unicode Text to jan.txt (replace jan with actual month, 
   see supported month list with bk months).
3. iconv -f UTF16LE -t UTF-8 jan.txt > jan
4. Create separate folder for each year and name it according to the 
   corresponding year.
5. bk import path/to/that/folder
