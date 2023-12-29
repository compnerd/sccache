
use crate::cache::{FileObjectSource, Storage};
use crate::compiler::{
  Cacheable,
  ColorMode,
  Compilation,
  CompileCommand,
  Compiler,
  CompilerArguments,
  CompilerHasher,
  CompilerKind,
  HashResult,
  Language,
};
use crate::compiler::args::{
  ArgInfo,
  ArgToStringResult,
  Argument,
  ArgsIter,
  IntoArg,
  PathTransformerFn,
};
use crate::compiler::c::ArtifactDescriptor;
use crate::counted_array;
use crate::dist;
use crate::errors::Result;
use crate::mock_command::CommandCreatorSync;
use crate::util::{Digest, HashToDigest};

#[cfg(feature = "dist-client")]
use crate::compiler::DistPackagers;
#[cfg(feature = "dist-client")]
use crate::dist::pkg;

use super::CacheControl;

use async_trait::async_trait;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::OsString;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Version number for cache key.
const CACHE_VERSION: &[u8] = b"1";

#[derive(Clone, Debug)]
pub struct ParsedArguments {
  /// The full command line, with all parsed arguments.
  pub arguments: Vec<Argument<ArgData>>,
  /// The value of any `-color-diagnostics`, `-no-color-diagnostics` argument
  /// passed on the commandline.
  pub color_mode: ColorMode,
  /// The module name passed to `-module-name`.
  pub module_name: String,
}

#[derive(Clone, Debug)]
pub struct Swift {
  /// The path to the swift-frontend executable.
  pub executable: PathBuf,
  /// The version string for this compiler.
  ///
  /// Looks like the following:
  /// ```shell
  /// :) swift-frontend --version
  /// compnerd.org Swift version 5.11-dev (LLVM 13124099c3f0229, Swift f08f86c71617bac)
  /// Target: aarch64-unknown-windows-msvc
  /// ```
  pub version: String,
  /// The host triple for this compiler.
  pub host: String,
  /// The path to the SDK.
  pub sdk: PathBuf,
}

ArgData! {
  Output(PathBuf),
  PassThroughFlag,
  PassThrough(OsString),
}

use self::ArgData::*;

pub struct SwiftCompilation {
  /// The path to the swift-frontend executable.
  pub executable: PathBuf,
  /// The host triple we are compiling for.
  pub host: String,
  /// The SDK for this compilation.
  pub sdk: PathBuf,
  /// All arguments passed to `swift-frontend`.
  pub arguments: Vec<Argument<ArgData>>,
  /// The compiler inputs.
  pub inputs: Vec<PathBuf>,
  /// The compiler outputs.
  pub outputs: HashMap<String, ArtifactDescriptor>,
  /// The module name passed to `-module-name`.
  pub module_name: String,
  /// The current working directory.
  pub cwd: PathBuf,
  /// Consumed environment variables.
  pub environment_variables: Vec<(OsString, OsString)>,
}

/// A struct on which to hang a `CompilerHasher` impl.
#[derive(Clone, Debug)]
pub struct SwiftHasher {
  /// The path to the swift-frontend executable.
  pub executable: PathBuf,
  /// The compiler version string.
  pub version: String,
  /// The host triple we are compiling for.
  pub host: String,
  /// The path to the SDK for this compilation.
  pub sdk: PathBuf,
  /// Parsed arguments from the swift-frontend invocation.
  pub parsed_argumments: ParsedArguments
}

#[cfg(feature = "dist-client")]
#[allow(unused)]
struct SwiftToolchainPackager {
  executable: PathBuf,
  kind: CompilerKind,
}

#[cfg(feature = "dist-client")]
struct SwiftInputsPackager {
  input_path: PathBuf,
  path_transformer: dist::PathTransformer,
}

impl Swift {
  pub async fn new<T: CommandCreatorSync>(mut _creator: T, executable: PathBuf,
                                          _env_vars: &[(OsString, OsString)],
                                          swiftc_version: &str,
                                          _dist_archive: Option<PathBuf>,
                                          _pool: tokio::runtime::Handle)
      -> Result<Swift> {
    // TODO(compnerd): extract the SDK and host information.
    Ok(Swift {
      executable: executable.clone(),
      version: swiftc_version.to_string(),
      host: "".into(),
      sdk: "".into(),
    })
  }
}

macro_rules! require {
  ($unwrapped:ident) => {
    let $unwrapped = if let Some($unwrapped) = $unwrapped {
      $unwrapped
    } else {
      debug!("cannot cache compilation, missing `{}`", stringify!($unwrapped));
      cannot_cache!(concat!("missing ", stringify!($unwrapped)));
    };
  };
}

counted_array!(static ARGS: [ArgInfo<ArgData>; _] = [
  flag!("-color-diagnostics", PassThroughFlag),
  flag!("-no-color-diagnostics", PassThroughFlag),
  // ArgInfo::new("module-name", true, false, ArgData::from),
]);

fn parse_arguments(arguments: &[OsString], cwd: &Path,
                   env_vars: &[(OsString, OsString)])
    -> CompilerArguments<ParsedArguments> {
  let mut parsed_arguments = vec![];
  let mut color_mode = ColorMode::Auto;
  let mut module_name = None;

  for argument in ArgsIter::new(arguments.iter().cloned(), &ARGS[..]) {
    let argument = try_or_cannot_cache!(argument, "argument parser");

    let mut input = None;

    match argument.get_data() {
      None => {
        match argument {
          Argument::Raw(ref value) => {
            input = Some(value.clone());
          }

          Argument::UnknownFlag(_) => {
            // Ignore unknown flags.
          }

          Argument::Flag(_, _) => {
          }

          Argument::WithValue(_, _, _) => {
          }
        }
      }

      Some(_) => todo!()
    }
  }

  require!(module_name);

  CompilerArguments::Ok(ParsedArguments {
    arguments: parsed_arguments,
    color_mode,
    module_name,
  })
}

impl<T: CommandCreatorSync> Compiler<T> for Swift {
  fn kind(&self) -> CompilerKind { CompilerKind::Swift }

  fn parse_arguments(&self, arguments: &[OsString], cwd: &Path,
                     env_vars: &[(OsString, OsString)])
      -> CompilerArguments<Box<dyn CompilerHasher<T> + 'static>> {
    match parse_arguments(arguments, cwd, env_vars) {
      CompilerArguments::Ok(arguments) => {
        CompilerArguments::Ok(Box::new(SwiftHasher {
          executable: self.executable.clone(),
          version: self.version.clone(),
          host: self.host.clone(),
          sdk: self.sdk.clone(),
          parsed_argumments: arguments,
        }))
      },
      CompilerArguments::CannotCache(why, extra_info) => {
        CompilerArguments::CannotCache(why, extra_info)
      },
      CompilerArguments::NotCompilation => {
        CompilerArguments::NotCompilation
      },
    }
  }

  fn box_clone(&self) -> Box<dyn Compiler<T>> {
    Box::new((*self).clone())
  }

  #[cfg(feature = "dist-client")]
  fn get_toolchain_packager(&self) -> Box<dyn pkg::ToolchainPackager> {
    Box::new(SwiftToolchainPackager {
      executable: self.executable.clone(),
      kind: CompilerKind::Swift,
    })
  }
}

#[async_trait]
impl<T: CommandCreatorSync> CompilerHasher<T> for SwiftHasher {
  async fn generate_hash_key(self: Box<Self>, _creator: &T, cwd: PathBuf,
                             env_vars: Vec<(OsString, OsString)>,
                             _may_dist: bool, pool: &tokio::runtime::Handle,
                             _rewrite_includes_only: bool,
                             _storage: Arc<dyn Storage>,
                             _cache_control: CacheControl)
      -> Result<HashResult> {

    let SwiftHasher {
      executable,
      version,
      host,
      sdk,
      parsed_argumments: ParsedArguments {
        arguments,
        color_mode,
        module_name,
      }
    } = *self;

    trace!("[{}]: generate_hash_key", module_name);

    let mut digest = Digest::new();

    // Hash Inputs:
    //  - A version key.
    digest.update(CACHE_VERSION);
    //  - Compiler dependencies.
    // TODO(compnerd): determine the compiler dependencies.

    let weak_toolchain_key = digest.clone().finish();

    //  - The full commandline.
    // TODO: this doesn't produce correct arguments if they should be concatenated - should use iter_os_strings
    let os_string_arguments: Vec<(OsString, Option<OsString>)> = arguments
        .iter()
        .map(|arg| {
          (
            arg.to_os_string(),
            arg.get_data().cloned().map(IntoArg::into_arg_os_string),
          )
        })
        .collect();

    //  - The digest of all source files
    //  - The digest of all files listed on the commandline.
    //  - The digest of all static libraries listed on the commandline.

    //  - The environment variables that the frontend reads.
    // TODO(compnerd): determine the environment variables that the frontend needs
    let mut environment_variables: Vec<_> = env_vars
        .iter()
        .cloned()
        .collect();

    //  - The current working directory of the compiler invocation.
    //  - The version of the compiler.
    version.hash(&mut HashToDigest { digest: &mut digest});

    Ok(HashResult {
      key: digest.finish(),
      compilation: Box::new(SwiftCompilation {
        executable,
        host,
        sdk,
        arguments,
        inputs: vec![],
        outputs: HashMap::new(),
        module_name,
        cwd,
        environment_variables,
      }),
      weak_toolchain_key,
    })
  }

  fn color_mode(&self) -> ColorMode {
    self.parsed_argumments.color_mode
  }

  fn output_pretty(&self) -> Cow<'_, str> {
    Cow::Borrowed(&self.parsed_argumments.module_name)
  }

  fn box_clone(&self) -> Box<dyn CompilerHasher<T>> {
    Box::new((*self).clone())
  }

  fn language(&self) -> Language {
    Language::Swift
  }
}

impl Compilation for SwiftCompilation {
  fn generate_compile_commands(&self,
                               _path_transformer: &mut dist::PathTransformer,
                               _rewrite_includes_only: bool)
      -> Result<(CompileCommand, Option<dist::CompileCommand>, Cacheable)> {
    Ok((
      CompileCommand {
        executable: self.executable.clone(),
        arguments: vec![],
        env_vars: self.environment_variables.clone(),
        cwd: self.cwd.clone(),
      },
      None,
      Cacheable::Yes,
    ))
  }

  fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = FileObjectSource> + 'a> {
    Box::new(self.outputs.iter().map(|(k, v)| {
      FileObjectSource {
        key: k.to_string(),
        path: v.path.clone(),
        optional: v.optional,
      }
    }))
  }

  #[cfg(feature = "dist-client")]
  fn into_dist_packagers(self: Box<Self>,
                         _path_transformer: dist::PathTransformer)
      -> Result<DistPackagers> {
    todo!()
  }
}

impl pkg::InputsPackager for SwiftInputsPackager {
  fn write_inputs(self: Box<Self>, wtr: &mut dyn std::io::Write)
      -> Result<dist::PathTransformer> {
    todo!()
  }
}
