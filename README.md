# Democracy & Economic Analysis Dashboard

An interactive R Shiny web application for analyzing democratic and economic trends across countries using large-scale historical datasets. Built with shinylive for serverless deployment and advanced statistical modeling capabilities.

## 🌟 Features

### Statistical Analysis
- **Bayesian Structural Time Series (BSTS)** modeling for trend analysis
- **Arrow plots** for visualizing directional changes in democracy metrics
- **Interactive time series visualizations** showing economic and political evolution
- **Cross-country comparative analysis** with dynamic filtering

### Data Integration
- **Maddison Project Dataset**: Historical economic indicators spanning centuries
- **V-Dem Dataset**: Comprehensive democracy measures for 180+ countries

### Technical Implementation
- **shinylive deployment**: Browser-based execution without server requirements
- **Interactive dashboards**: Real-time data filtering and visualization updates
- **Responsive design**: Optimized for various screen sizes and devices

## 📊 Datasets

### The Maddison Project
Historical statistics on world population, GDP, and per capita GDP spanning from 1 CE to present day across major world regions.

### Varieties of Democracy (V-Dem)
Comprehensive dataset measuring democratic institutions and processes with 450+ indicators across 200+ countries from 1789-2023.

## 🚀 Live Demo

[View Live Application](https://euhystho.github.io/shinytesting/) *(It may take a while to load...)*

## 💻 Technologies Used

- **R Shiny**: Interactive web application framework
- **shinylive**: Client-side deployment enabling serverless hosting
- **ggplot2**: Statistical data visualization
- **dplyr**: Data manipulation and analysis
- **BSTS**: Bayesian Structural Time Series modeling
- **plotly**: Interactive plotting capabilities

## 📈 Key Visualizations

1. **Democracy Evolution Arrow Plots**: Directional changes in democratic indicators over time
2. **Economic Growth Trajectories**: GDP per capita trends with uncertainty intervals
3. **Comparative Dashboard**: Side-by-side country analysis with customizable metrics
4. **Correlation Analysis**: Relationships between economic development and democratic progress

## 🔍 Analysis Insights

- Identification of democratization waves and reversals
- Economic development patterns and their correlation with political systems
- Regional trends in governance and economic growth
- Forecasting capabilities for democratic and economic indicators

## 📁 Project Structure

```
shiny.old/
├── app.R                 # Main Shiny application
├── datasets/                 # Processed datasets
│   ├── maddison_data.rds
│   └── vdem_data.rds
├── R/                    # Helper functions
│   ├── arrow_module.R
│   ├── democracy_module.R
│   ├── image_module.R
│   └── countries_module.R
└── www/                  # Static assets
    └── styles.css
```

## 🎯 Future Enhancements

- [ ] Machine learning predictions for democratic transitions
- [ ] Additional economic indicators (inequality, trade)
- [ ] Adding the Human Freedom Index
- [ ] Real-time data updates via APIs
- [ ] Export functionality for analysis results
- [ ] Multi-language support

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📧 Contact

For questions or collaboration opportunities, reach out via [LinkedIn](your-linkedin-profile) or [email](your-email).

---

*Built with ❤️ using R Shiny and modern statistical techniques*
