# Brings the Explorer window of -Path to the foreground.
# Windows blocks SetForegroundWindow for background processes (rsession);
# a synthetic ALT key press lifts this lock.
param([string]$Path)
Add-Type @"
using System;
using System.Runtime.InteropServices;
public class SRWin {
  [DllImport("user32.dll")] public static extern bool SetForegroundWindow(IntPtr h);
  [DllImport("user32.dll")] public static extern bool ShowWindow(IntPtr h, int n);
  [DllImport("user32.dll")] public static extern void keybd_event(byte k, byte s, uint f, UIntPtr e);
}
"@
$shell = New-Object -ComObject Shell.Application
for ($i = 0; $i -lt 30; $i++) {
  $win = $shell.Windows() |
    Where-Object { $_.LocationURL -and ([uri]$_.LocationURL).LocalPath -eq $Path } |
    Select-Object -First 1
  if ($win) {
    $hwnd = [IntPtr]$win.HWND
    [SRWin]::keybd_event(0x12, 0, 0, [UIntPtr]::Zero)
    [SRWin]::keybd_event(0x12, 0, 2, [UIntPtr]::Zero)
    [void][SRWin]::ShowWindow($hwnd, 9)
    [void][SRWin]::SetForegroundWindow($hwnd)
    break
  }
  Start-Sleep -Milliseconds 100
}
