@echo Archiving PMOS/2 to drive I:
copy pmos2.zip pmos2.zip.bak
copy I:pmos2.zip
zip -r -o -u -X pmos2.zip . -x *.red *.bak *.obj *.rsp *.map *.exe *.sym *.db* *.$$$ z*.cmd
copy pmos2.zip I:
