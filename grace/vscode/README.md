# Enable VSCode syntax highlighting

## Prerequisites

- npm

### For Linux Users:

Download `npm` by simply executing:

```bash
sudo apt install npm
```

### For Windows Users:

Download `npm` from the official website [here](https://nodejs.org/en/download).

## Folder creation

### For Linux Users:

```bash
npm init -y
sudo npm install -g yo generator-code
yo code
```

### For Windows Users:

```bash
npm init -y
npm install -g yo generator-code
yo code
```

## Complete YO Configuration

### For Linux Users:

Complete the `yo code` as we suggest in the picture below:

![Screenshot from 2023-03-21 18-23-44](https://user-images.githubusercontent.com/56654089/226679987-7c045192-9217-4815-95ac-7c8c944826d6.png)

### For Windows Users:

Complete the `yo code` configuration as shown in the picture above with the only change being the name of the extension. Instead of `grace_vscode_syntax_highlighting` you should enter `grace_vscode_syntax_highlighting-1.0.0`. Everything else will work just fine.

## Enable Extension

### For Linux Users:

Finally copy the folder `grace_vscode_syntax_highlighting` and paste it inside folder `~/.vscode/extensions`.

### For Windows Users:

Copy the folder `grace_vscode_syntax_highlighting-1.0.0` and paste it inside the folder `C:\Users\{your-pc-name}\.vscode\extensions`.

## Specify the syntax of Grace

Copy the contents of the files `syntaxes/grc.tmLanguage.json` and `language-configuration.json` and paste them in the corresponding directories of your extension folder. Feel free to add your own improvements to the syntax highlighting by editing the two specified files above.

## Clear unnecessary files (_optional_)

Additionally you may want to remove/delete any unnecessary files that were created at this process, such as:

- README.md
- CHANGELOG.md
- vsc-extensions-quickstart.md

## Final step

Enjoy syntax highlighting for the Grace Programming Language!

<sub>**Hint**: Consider creating a soft link to point to the folder of this repository, in order to update the syntax highlighter easier by pulling any changes from github.</sub>
