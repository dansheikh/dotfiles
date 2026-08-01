Write-Host "Installing Visual Studio Code extensions...\n"

Get-Content extensions.txt | ForEach-Object { code --install-extension $_ }

Write-Host "Installing Visual Studio Code extensions complete...\n"
