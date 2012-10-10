import  random
import  time
import  thread

class MyThread:
    def Start(self, index):
        thread.start_new_thread(self.Run, ())
        self.index = index

    def Run(self):
        i = 1
        while 1:
            print "Thread ", self.index, ": ", i
            i = i + 1
            sleeptime = (random.random() * 2) + 0.5
            time.sleep(sleeptime/4)

t1 = MyThread()
t1.Start(1)

MyThread().Start(2)

while 1: pass
