pub enum Arch {
    X86,
    X86_64,
    Wasm32,
    Wasm64,
    Arm,
    Aarch64,
    RiscV,
    Mips,
    PowerPc,
    Sparc,
    Z390,
    M68k,
    SpirV,
    RiscV32,
    RiscV64,
    RiscV128,
}

pub enum Os {
    BareMetal,
    Windows,
    Linux,
    Bsd,
    Solaris,
    Illumos,
    Haiku,
    Redox,
    Theseus,
}

pub struct Target {
    pub arch: Arch,
    pub os: Os,
}

impl Target {
    pub fn new(arch: Arch, os: Os) -> Self {
        Self { arch, os }
    }
}
