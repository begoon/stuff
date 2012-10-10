import serial

def exec_cmd(port, cmd):
   print "Exec '%s' command" % cmd
   port.write(cmd + "\r")
   while 1:
      line = port.readline()
      print line

      if line.startswith("OK") or line.startswith("ERROR"): 
         break

port = serial.Serial('COM3', timeout=2)
print port

exec_cmd( port, "ati4" )

port.close()
