require './as80'

class Radio86 < I8080
  def self.video_mem
    0x76D0
  end
  def self.monitor_putc(cmd = nil)
    push psw
    eval cmd if cmd != nil
    call 0xf809
    pop psw
  end

  def self.monitor_puts(msg = nil)
    push psw
    if msg then
      push h
      lxi h, msg
      pop h
    end
    call 0xf818
    pop psw
  end
end
