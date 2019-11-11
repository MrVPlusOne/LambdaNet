// All: 11 JSNice: 4 LambdaNet: 8

export enum Shell {
    PowerShell,
    Cmd,
    Shell,
    Wsl
}

export function escapeSpaces(s: string, shell: Shell): string {
    if (!s.includes(' ')) {
        return s;
    }
    switch (shell) {
        case Shell.PowerShell:
            // Unescape
            s = s.replace(new RegExp('` ', 'g'), ' ');
            // Escape
            return s.replace(new RegExp(' ', 'g'), '` ');
        case Shell.Cmd:
            s = s.concat();
            if (!s.startsWith('"')) {
                s = '"'.concat(s);
            }
            if (!s.endsWith('"')) {
                s = s.concat('"');
            }
            return s;
        case Shell.Shell:
        case Shell.Wsl:
            s = s.concat();
            if (!s.startsWith('\'')) {
                s = '\''.concat(s);
            }
            if (!s.endsWith('\'')) {
                s = s.concat('\'');
            }
            return s;
    }
}


export function getCommandForArgs(shell: Shell, args: string[]): string {
    args = args.map(a => escapeSpaces(a, shell));
    return args.join(' ');
}

function correctPath(path: string): string {
    const disk = path.substr(0, 1).toLowerCase(); // For `C:\\Directory` it will be `C`
    path = path.replace(new RegExp('\\\\', 'g'), '/'); // After the line it will look like `C:/Directory`
    const pathWithoutDisk = path.substring(path.indexOf('/') + 1); // For `C:/Directory` it will be `Directory`
    return `/mnt/${disk}/${pathWithoutDisk}`;
}

export function getCommandToChangeWorkingDirectory(
    shell: Shell,
    workingDirectory: string
): string {
    if (shell === Shell.Wsl) {
        workingDirectory = correctPath(workingDirectory);
    }
    return getCommandForArgs(shell, ['cd', workingDirectory]);
}
