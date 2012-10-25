class I8080
  Regs8 = ["b", "c", "d", "e", "h", "l", "m", "a"]
  Regs16 = ["b", "d", "h", "sp"]
  StackRegs = ["b", "d", "h", "psw"]

  Conds = ["nz", "z", "nc", "c", "po", "pe", "p", "m"]
  CondPrefix = { "j" => 0xc2, "r" => 0xc0, "c" => 0xc4 }

  Keywords = Regs8 | Regs16 | StackRegs

  @@image = []
  @@regs_stack = []
  @@unresolved = {}
  
  def self.method_missing name, *args
    name = name.to_s
    if Keywords.include?(name) then
      return name
    end

    CondPrefix.each do | prefix, opcode |
      Conds.each do |c|
        cmd = prefix + c
        if cmd == name then 
          opcode = opcode | (Conds.index(c) << 3)
          if prefix == "r" then
            self.cmd1(opcode, cmd)
          else
            self.cmd3_16(opcode, args[0], cmd)
          end
          return nil
        end
      end
    end
    puts "LABEL: %s [%s]" % [name, args.inspect]
    return "??LABEL_" + name
  end

  def self.org(addr)
    puts "org %s" % addr
  end

  def self.reg(r, values, method)
    i = values.index(r)
    raise "Invalid parameter '%s' in '%s'" % [r, method] if i == nil
    return i
  end

  def self.here
    return @@image.length
  end

  def self.emit(mnemonic, opcode, sz = 1, arg = nil)
    @@image << opcode
    if sz > 1 then
      if arg.class.name == "String" && arg.start_with?("??LABEL_") then
        arg = 0
        @@unresolved[arg] = [] if not @@unresolved.has_key? arg
        @@unresolved[arg] << self.here
        puts @@unresolved.inspect
      end
      @@image << (arg & 0xff)
      @@image << (arg >> 8) if sz > 2
    end
    p1 = sz > 1 ? ("%02X" % (arg & 0xff)) : "  "
    p2 = sz > 2 ? ("%02X" % (arg >> 8)) : "  "
    lst = "%04X %02X %s %s %s" % [@@image.length-1, opcode, p1, p2, mnemonic]
    puts lst
  end

  def self.cmd1(opcode, name)
    emit(name, opcode)
  end

  def self.hlt
    self.cmd1(0x76, __method__)
  end

  def self.nop
    self.cmd1(0x00, __method__)
  end

  def self.ret
    self.cmd1(0xc9, __method__)
  end

  def self.xchg
    self.cmd1(0xeb, __method__)
  end

  def self.sphl
    self.cmd1(0xf9, __method__)
  end

  def self.xthl
    self.cmd1(0xe3, __method__)
  end

  def self.cma
    self.cmd1(0x2f, __method__)
  end

  def self.rlc
    self.cmd1(0x07, __method__)
  end

  def self.rrc
    self.cmd1(0x0f, __method__)
  end

  def self.ral
    self.cmd1(0x17, __method__)
  end

  def self.rar
    self.cmd1(0x1f, __method__)
  end

  def self.ei
    self.cmd1(0xfb, __method__)
  end

  def self.di
    self.cmd1(0xf3, __method__)
  end

  def self.stc
    self.cmd1(0x37, __method__)
  end

  def self.cmc
    self.cmd1(0x3f, __method__)
  end

  def self.pchl
    self.cmd1(0xe9, __method__)
  end

  def self.daa
    self.cmd1(0x27, __method__)
  end

  def self.cmd2_reg(opcode, r, im8, name)
    p1 = self.reg(r, Regs8, __method__)
    opcode = opcode | (p1 << 3)
    asm = "%s %s, %s" % [name, r, im8.to_s]
    emit(asm, opcode, 2, im8)
  end

  def self.mvi(r, im8)
    self.cmd2_reg(0x06, r, im8, __method__)
  end

  def self.cmd1_reg(opcode, r, name, shift = 0, regs = Regs8)
    p1 = self.reg(r, regs, __method__)
    opcode = opcode | (p1 << shift)
    emit("%s %s" % [name, r], opcode)
  end

  def self.inx(r)
    self.cmd1_reg(0x03, r, __method__, 4, Regs16)
  end

  def self.dcx(r)
    self.cmd1_reg(0x0b, r, __method__, 4, Regs16)
  end

  def self.inr(r)
    self.cmd1_reg(0x04, r, __method__, 3)
  end

  def self.dcr(r)
    self.cmd1_reg(0x05, r, __method__, 3)
  end

  def self.add(r)
    self.cmd1_reg(0x80, r, __method__)
  end

  def self.adc(r)
    self.cmd1_reg(0x88, r, __method__)
  end

  def self.sub(r)
    self.cmd1_reg(0x90, r, __method__)
  end

  def self.sbb(r)
    self.cmd1_reg(0x98, r, __method__)
  end

  def self.ana(r)
    self.cmd1_reg(0xa0, r, __method__)
  end

  def self.xra(r)
    self.cmd1_reg(0xa8, r, __method__)
  end

  def self.ora(r)
    self.cmd1_reg(0xb0, r, __method__)
  end

  def self.cmp(r)
    self.cmd1_reg(0xb8, r, __method__)
  end

  def self.cmd2_im8(opcode, im8, name)
    emit("%s %s" % [name, im8.to_s], opcode, 2, im8)
  end

  def self.in(im8)
    self.cmd2_im8(0xdb, im8, __method__)
  end

  def self.out(im8)
    self.cmd2_im8(0xd3, im8, __method__)
  end

  def self.adi(im8)
    self.cmd2_im8(0x80|0x46, im8, __method__)
  end

  def self.aci(im8)
    self.cmd2_im8(0x88|0x46, im8, __method__)
  end

  def self.sui(im8)
    self.cmd2_im8(0x90|0x46, im8, __method__)
  end

  def self.sbi(im8)
    self.cmd2_im8(0x98|0x46, im8, __method__)
  end

  def self.ani(im8)
    self.cmd2_im8(0xa0|0x46, im8, __method__)
  end

  def self.xri(im8)
    self.cmd2_im8(0xa8|0x46, im8, __method__)
  end

  def self.ori(im8)
    self.cmd2_im8(0xb0|0x46, im8, __method__)
  end

  def self.cpi(im8)
    self.cmd2_im8(0xb8|0x46, im8, __method__)
  end

  def self.mov(r1, r2)
    p1 = self.reg(r1, Regs8, __method__)
    p2 = self.reg(r2, Regs8, __method__)
    opcode = 0x40 | (p1 << 3) | p2
    emit("mov %s, %s" % [r1, r2], opcode)
  end

  def self.lxi(rp, im16)
    p1 = self.reg(rp, Regs16, __method__)
    opcode = 0x01 | (p1 << 4)
    emit("lxi %s, %s" % [rp, im16.to_s], opcode, 3, im16)
  end

  def self.cmd1_rp(opcode, rp, name, regs = Regs16)
    p1 = self.reg(rp, regs, name)
    opcode = opcode | (p1 << 4)
    emit("%s %s" % [name, rp], opcode)
  end

  def self.ldax(rp)
    self.cmd1_rp(0x0a, rp, __method__)
  end

  def self.stax(rp)
    self.cmd1_rp(0x02, rp, __method__)
  end

  def self.dad(rp)
    self.cmd1_rp(0x09, rp, __method__)
  end

  def self.push(rp)
    self.cmd1_rp(0xc5, rp, __method__, StackRegs)
  end

  def self.pop(rp)
    self.cmd1_rp(0xc1, rp, __method__, StackRegs)
  end

  def self.cmd3_16(opcode, im16, name)
    emit("%s %s" % [name, im16.to_s], opcode, 3, im16)
  end

  def self.jmp(im16)
    self.cmd3_16(0xc3, im16, __method__)
  end

  def self.call(im16)
    self.cmd3_16(0xcd, im16, __method__)
  end

  def self.sta(im16)
    self.cmd3_16(0x32, im16, __method__)
  end

  def self.lda(im16)
    self.cmd3_16(0x3a, im16, __method__)
  end

  def self.lhld(im16)
    self.cmd3_16(0x2a, im16, __method__)
  end

  def self.shld(im16)
    self.cmd3_16(0x22, im16, __method__)
  end

  def self.rst(n)
    raise "Invalid rst parameter '%d'" % n if not (0..7).include?(n)
    opcode = 0xc7 | (n << 3)
    emit("rst %s" % n.to_s, opcode)
  end

  def self.store(*regs)
    @@regs_stack << regs
    regs.each { |r| self.push(r) }
  end

  def self.restore
    raise "Register stack is empty" if @@regs_stack.empty?
    regs = @@regs_stack.pop
    regs.reverse.each { |r| self.pop(r) }
  end

  def self.binary(name)
    puts "Image size: %04X (%d)" % [@@image.length, @@image.length]
    puts @@image.inspect
    bin = @@image.pack("C*")
    
    IO.binwrite(name, bin)
  end
end
