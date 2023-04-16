## Enable VSCode syntax highlighting

<sub>**Disclaimer**: The instructions given below are tested and working on Linux</sub>

Bash commands:

```bash
sudo apt install npm
npm init -y
sudo npm install -g yo generator-code
yo code
```

Now complete the `yo code` as we suggest in the picture below:

![Screenshot from 2023-03-21 18-23-44](https://user-images.githubusercontent.com/56654089/226679987-7c045192-9217-4815-95ac-7c8c944826d6.png)


Finally copy the folder `grace_vscode_syntax_highlighting` and paste it inside folder `~/.vscode/extensions`. Additionally you may want to remove/delete any unnecessary files that were created at this process.


<sub>**Hint**: Consider creating a soft link to point to the folder of this repository, in order to update the syntax highlighter easier by pulling any changes from github</sub>