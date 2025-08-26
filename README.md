# STshiny
STshiny is an interactive platform built on R/Shiny for visualization and analysis of spatial transcriptome data. The platform integrates standardization, dimensionality reduction clustering, marker gene identification, GO/KEGG enrichment, cell annotation, scRNA-seq integration and other functions. It is easy to operate and has rich visualization results.

# Usage

We provide two ways to use STshiny:

## Online version 

No need to install a local environment, just open the link to use it!

https://xulabgdpu.cpolar.top/STshiny/

## STshiny container deployment instructions
### Project Introduction
`STshiny.sif` is an Apptainer container for running STshiny Shiny applications. STshiny is a web tool for spatial transcriptomics data analysis and visualization, supporting Seurat object analysis. This container provides a ready-made environment that can help users quickly deploy and run STshiny applications without manually configuring complex environment dependencies.
### Environmental Requirements

#### 1.1 Enable WSL2
Open Control Panel → Turn Windows features on or off.
Select:

✅ Windows Subsystem for Linux

✅ Virtual Machine Platform

Restart your computer.
#### 1.2 Install Ubuntu
Open the Microsoft Store and install Ubuntu 20.04 LTS or Ubuntu 22.04 LTS.
Set your username and password on the first boot.
#### 1.3 Install AppContainer
Run the following command in the Ubuntu terminal:
```bash
sudo apt update
sudo apt install -y apptainer
```
Check whether the installation was successful:
```bash
apptainer --version
```
### Obtain the container and script
#### 2.1 Place [STshiny.tar.gz](https://drive.google.com/file/d/1QidApVHVUPPlZHM3cLIUu4Ryy3RHmM0J/view?usp=drive_link), [STshiny.sif](https://drive.google.com/file/d/1hcl4qEL8goQ9wjVwmSJTE34W75vkg3jj/view?usp=drive_link), and [run-STshiny.sh](https://drive.google.com/file/d/1ENhTAPqjS9MPtsz6FCDr6IfKvHfftraz/view?usp=drive_link) in the same directory, for example:
```bash
D:/STshiny/
```
#### 2.2 In Ubuntu, go to this directory:
```bash
cp /mnt/d/STshiny/STshiny.sif ~/
cp /mnt/d/STshiny/STshiny.tar.gz ~/
cp /mnt/d/STshiny/run-STshiny.sh ~/
cd ~
```
### Running a Shiny App
#### 3.1 Add execution permissions to the script (only required for the first run):
```bash
chmod +x run-STshiny.sh
```
#### 3.2 Start the application:
```bash
./run-STshiny.sh
```
During the running process, the script will automatically call Apptainer to start STshiny.sif and run Shiny.

After successful startup, the terminal will output a prompt similar to this:
```bash
Listening on http://127.0.0.1:8080
```
Copy the output URL, paste it into a Windows browser and open it.

If you can see the STshiny interface, it means the application is running successfully.

