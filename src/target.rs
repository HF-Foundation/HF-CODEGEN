#[derive(Debug, Clone, PartialEq, Copy)]
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

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CallingConvention {
    /// The System V calling convention is used on most 32-bit Unix-like systems
    X86_CDeclGcc,
    /// cdecl Windows variant
    X86_CDeclWindows,
    /// 32 bit OS/2 API
    X86_Syscall,
    /// Microsoft __fastcall
    X86_Fastcall,
    /// Microsoft x64 (used instead of fastcall, thiscall, cdecl, etc. on x64 windows)
    /// Technically, vectorcall exists, but we dont care.
    X86_64_MicrosoftX64,
    /// System V AMD64 ABI (default on x64 unix systems)
    X86_64_SystemVAMD64,
}

impl CallingConvention {
    pub fn from_arch_os(arch: Arch, os: Os) -> CallingConvention {
        match os {
            Os::Windows => match arch {
                Arch::X86 => CallingConvention::X86_CDeclWindows,
                Arch::X86_64 => CallingConvention::X86_64_MicrosoftX64,
                _ => todo!(),
            },
            Os::Linux => match arch {
                Arch::X86 => CallingConvention::X86_CDeclGcc,
                Arch::X86_64 => CallingConvention::X86_64_SystemVAMD64,
                _ => todo!(),
            },
            _ => todo!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
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
            calling_convention: CallingConvention::from_arch_os(Arch::X86_64, Os::Windows),
        };

        #[cfg(target_os = "linux")]
        return Self {
            arch: Arch::X86_64,
            calling_convention: CallingConvention::SystemV,
        };

        #[cfg(not(any(target_os = "windows", target_os = "linux")))]
        panic!("Unsupported target os!")
    }
}
