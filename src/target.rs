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

pub enum CallingConvention {
    SystemV,
    MicrosoftX64,
    Cdecl,
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
    pub calling_convention: CallingConvention,
}

impl Target {
    pub fn new(arch: Arch, calling_convention: CallingConvention) -> Self {
        Self {
            arch,
            calling_convention,
        }
    }

    pub fn native() -> Self {
        #[cfg(target_os = "windows")]
        return Self {
            arch: Arch::X86_64,
            calling_convention: CallingConvention::MicrosoftX64,
        };

        #[cfg(target_os = "linux")]
        return Self {
            arch: Arch::X86_64,
            calling_convention: CallingConvention::SystemV,
        };

        panic!("Unsupported target os!")
    }
}
