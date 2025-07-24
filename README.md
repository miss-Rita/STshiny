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
- [Apptainer](https://apptainer.org/docs/user/main/quick_start.html) 1.0 or higher
- Conda environment (already included in the container)

If you do not have administrator privileges (cannot use sudo), you can install Apptainer in user mode to run the container:
```bash
# Download and unzip the source code
wget https://github.com/apptainer/apptainer/releases/download/v1.2.5/apptainer-1.2.5.tar.gz
tar -xzf apptainer-1.2.5.tar.gz
cd apptainer-1.2.5

#Configure compilation to the user directory
./mconfig --prefix=$HOME/apptainer
cd builddir
make -j4
make install
```
Add environment variables (can be written to .bashrc or .zshrc):
```bash
export PATH="$HOME/apptainer/bin:$PATH"
export APPTAINER_CACHEDIR=$HOME/.apptainer/cache
```

### Usage steps

#### 1. Download the container

First, download the [STshiny.sif](https://drive.google.com/file/d/1ft0aPGPHieA3wpvTImTaq6aWkAgeXpoH/view?usp=sharing) container file to your local computer.

#### 2. Start the container

Enter the container using the following command:

```bash
apptainer shell --writable --fakeroot STshiny.sif
```

#### 3. Set environment variables
After entering the container, execute the following command to set the required environment variables:
```bash
export PATH=/root/.conda/envs/apptainer/bin:$PATH
export R_LIBS_USER=/usr/lib/R/site-library
export LC_ALL=C
```

#### 4. Get the local IP address
The container will automatically get the local IP address for Shiny application access:
```bash
IP_ADDRESS=$(hostname -I | awk '{print $1}')
```
#### 5. Restore R environment dependencies
To ensure that the R environment in the container has restored the required dependencies, execute the following command:
```bash
cd /root/jierhuang/apptainer/STshiny
Rscript -e 'renv::restore()'
```
#### 6. Start the Shiny application
Run the following command to start the Shiny application:
```bash
Rscript -e "shiny::runApp('/root/jierhuang/apptainer/miniconda_latest/srv/shiny-server/STshiny/', host = '$IP_ADDRESS', port = 8080, launch.browser = FALSE)"
```
This will start the STshiny application inside the container, listening on port 8080.

#### 7. Access the application
After launching, you can access the Shiny application in your browser through the following URL:
```cpp
http://<your-ip-address>:8080
```
Replace <your-ip-address> with the IP address you obtained inside the container.

### Notes
Make sure the Conda environment in the container is correctly configured and all R dependencies are restored successfully.

Before starting the application, make sure port 8080 is not occupied by other processes.

When starting the application, the browser will not be opened automatically by default, but you can manually access the link provided above.


# Data file description

If you want to download the code for this project locally and run it using RStudio, there are some sample data available here. Please download them and place them in the main directory of the project so that you can run the sample code smoothly.

[Click here to download all_markers.rds](https://drive.google.com/file/d/1MnQSfd8r8uHKpd7MtJUiezw93l161RcU/view?usp=drive_link)

[Click here to download brain_data.rds](https://drive.google.com/file/d/1s1u45Byk___xBGXhSobNlO3T0W8j7AN2/view?usp=drive_link)

[Click here to download brain_SCTransform.rds](https://drive.google.com/file/d/1e7_gaXrAnIuiyRnUh6Gzb2fm9K17Nz8i/view?usp=drive_link)

[Click here to download allen_reference.rds](https://drive.google.com/file/d/1gdrb94g3CPkFEmCEGtRAhYx5Mlvlp5mf/view?usp=drive_link)

Before running the program, make sure the file has been correctly downloaded to the specified path.





