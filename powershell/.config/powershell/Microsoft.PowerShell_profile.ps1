[Environment]::SetEnvironmentVariable("ALTERNATE_EDITOR", "nvim")
[Environment]::SetEnvironmentVariable("COURSIER_BIN_DIR", "${HOME}/.local/share/coursier/bin")
[Environment]::SetEnvironmentVariable("COURSIER_JVM_CACHE", "${HOME}/.cache/coursier/jvm")
[Environment]::SetEnvironmentVariable("DOTNET_TOOLS", "${HOME}/.dotnet/tools")
[Environment]::SetEnvironmentVariable("DOTNET_CLI_TELEMETRY_OPTOUT", 1)
[Environment]::SetEnvironmentVariable("EDITOR", "emacsclient -c -a `"`"")
[Environment]::SetEnvironmentVariable("GOPATH", (Invoke-Expression "${HOME}/.nix-profile/bin/go env GOPATH"))
[Environment]::SetEnvironmentVariable("LANG", "en_US.UTF-8")
[Environment]::SetEnvironmentVariable("LC_TYPE", "en_US.UTF-8")
[Environment]::SetEnvironmentVariable("MIX_XDG", "true")
[Environment]::SetEnvironmentVariable("XDG_CACHE_HOME", "${HOME}/.cache")
[Environment]::SetEnvironmentVariable("XDG_CONFIG_HOME", "${HOME}/.config")
[Environment]::SetEnvironmentVariable("XDG_DATA_HOME", "${HOME}/.local/share")
[Environment]::SetEnvironmentVariable("XDG_STATE_HOME", "${HOME}/.local/state")
[Environment]::SetEnvironmentVariable("MPLCONFIGDIR", "${XDG_CONFIG_HOME}/matplotlib")
[Environment]::SetEnvironmentVariable("POETRY_CACHE_DIR", "${XDG_CACHE_HOME}/pypoetry")
[Environment]::SetEnvironmentVariable("POETRY_CONFIG_DIR", "${XDG_CONFIG_HOME}/pypoetry")
[Environment]::SetEnvironmentVariable("POETRY_DATA_DIR", "${XDG_DATA_HOME}/pypoetry")
[Environment]::SetEnvironmentVariable("SDKMAN_DIR", "${HOME}/.sdkman")
[Environment]::SetEnvironmentVariable("VISUAL", "emacsclient -c -a `"`"")
$path = [Environment]::GetEnvironmentVariable("PATH")
if ([string]::IsNullOrEmpty($path)) {
  $path = "${HOME}/.local/bin:${Env:GOPATH}/bin:${HOME}/.nix-profile/bin"
} else {
  $path = "${HOME}/.local/bin:${Env:GOPATH}/bin:${HOME}/.nix-profile/bin:${path}"
}
[Environment]::SetEnvironmentVariable("PATH", "${path}")

# if ((Get-Command tmux | Out-Null) && [string]::IsNullOrEmpty(${Env:TMUX}) && [string]::IsNullOrEmpty(${Env:INSIDE_EMACS}) && -not (tmux has-session 2>&1 | Out-Null && $?)) {
#   tmux new -A
# }

if ("$TERM_PROGRAM" -ne "Apple_Terminal") {
  Invoke-Expression "$(oh-my-posh init pwsh --config ${Env:XDG_CONFIG_HOME}/oh-my-posh/oh-my-posh.toml)"
}

Invoke-Expression "$(direnv hook pwsh)"
