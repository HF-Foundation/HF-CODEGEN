#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum CallingConvention {
    SystemV,
    MicrosoftX64,
    Cdecl,
}

#[derive(Debug, Clone, PartialEq)]
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

impl Os {
    pub fn calling_convention(&self) -> CallingConvention {
        match self {
            Os::Windows => CallingConvention::MicrosoftX64,
            Os::Linux => CallingConvention::SystemV,
            Os::Bsd => CallingConvention::SystemV,
            Os::Solaris => CallingConvention::SystemV,
            Os::Illumos => CallingConvention::SystemV,
            Os::Haiku => CallingConvention::SystemV,
            Os::Redox => CallingConvention::SystemV,
            Os::Theseus => CallingConvention::SystemV,
            Os::BareMetal => CallingConvention::Cdecl,
        }
    }
}

#[derive(Debug, Clone)]
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
