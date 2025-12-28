# Romanian-Forest-and-Bear-Conflict-Analysis-1990---2019-
This project analyzes forest growth trends in Romania (1990-2019) and correlates them with bear-human conflict data. The analysis provides regional insights for environmental policy and wildlife management.

# Prerequisites
Docker Desktop (Install and start it)

All included in Docker - no manual installation needed
- R 4.3.0
- R packages: dplyr, ggplot2, tidyr, maps, mapdata
- Docker Engine 20.10+

# Setup Guide - First Time Setup
1. Open terminal/command prompt
   
2. Clone or download the project
   
git clone https://github.com/Alexia220700/forests-analysis.git
OR download ZIP and extract

3. Navigate to project folder
   
cd forests-analysis

6. Build Docker image (first time only - takes 5-10 minutes)
docker build -t forest-analysis .

7. Run the analysis
docker run -v "$(pwd)/outputs:/app/outputs" forest-analysis

# Expected Outputs
✓ Data preprocessing complete
✓ Forest growth analysis complete
✓ Livestock damage analysis complete
✓ All visualizations complete
✓ Analysis completed successfully
