# Logapp - Expense Tracker

<div align="center">

**A professional procurement and expense tracking system for ACTED logistics operations**

![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3?logo=r&logoColor=white)
![SQLite](https://img.shields.io/badge/SQLite-Database-003B57?logo=sqlite&logoColor=white)
![Status](https://img.shields.io/badge/Status-Production-success)

</div>

---

## 📋 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Architecture](#architecture)
- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)
- [Authentication](#authentication)
- [Database](#database)
- [Project Structure](#project-structure)
- [Development](#development)
- [Testing](#testing)
- [Deployment](#deployment)
- [Documentation](#documentation)

---

## 🎯 Overview

**Logapp** is a comprehensive web-based expense tracking and procurement management system built with R Shiny. Designed specifically for ACTED logistics operations, it provides a complete workflow for managing procurement requests from submission through approval, payment tracking, and financial reporting.

### Purpose

Logistics and administrative staff use Logapp to:
- Register and track procurement requests
- Review and approve expenses
- Monitor procurement progress
- Mark requests as paid
- Generate financial reports and deductions
- Manage project budgets and budget lines
- Track supplier relationships
- Monitor organizational spending trends

### Key Capabilities

- **Request Management**: Complete lifecycle tracking from submission to payment
- **Multi-Currency Support**: Handle transactions in ETB, USD, EUR, and GBP
- **Real-Time Dashboards**: Visual insights into spending patterns and request status
- **Financial Updates**: Budget-level checkpoint model for tracking deductions
- **Export Functionality**: Generate Excel reports for external finance systems

---

## ✨ Features

### 🔐 Secure Authentication
- Credential-based login system powered by shinymanager
- Clean, professional login page with Logapp branding
- Session management with automatic logout
- Secure password handling

### 📊 Request Management
- **Create & Edit**: Comprehensive request form with all necessary fields
- **Status Tracking**: Under Review → Pending → Paid → Cancelled workflow
- **Filtering**: Advanced search and filter options by status, project, budget line, date, person
- **Bulk Operations**: Export filtered requests to Excel
- **Real-Time Cards**: Dashboard showing request counts by status

### 💰 Financial Updates
- Budget-level checkpoint system
- USD batch processing for periodic financial reconciliation
- Automatic ETB/USD conversion using Central Bank of Ethiopia (CBE) rates
- Track paid deductions by project and budget line
- Export financial batches for external accounting

### 📈 Insights Dashboard
- **Spending Trends**: Visualize expenditure over time (daily, monthly, quarterly, yearly)
- **Breakdown Analysis**: Spending by project, budget line, requester, or department
- **Year Comparison**: Side-by-side annual spending comparison
- **Quarter Comparison**: Cross-year quarterly analysis
- **Processing Time**: Track days to complete vs. expected lead time

### 🛠️ Management Tools
- **Project & Budget Line Catalog**: Maintain organizational structure
- **Supplier Registry**: Track vendor relationships
- **People Management**: Maintain staff/contact lists
- **Email Notifications**: Configure weekly summary recipients
- **Default Settings**: Set default values for new requests

### 🎨 User Experience
- **Light/Dark Mode**: Toggle between themes
- **Responsive Design**: Works on desktop and tablet devices
- **Currency Selector**: Global currency display preference
- **Modern UI**: Clean, professional interface with bslib components

---

## 🏗️ Architecture

### Technology Stack

- **Frontend**: R Shiny with bslib for modern UI components
- **Backend**: R (statistical computing and data processing)
- **Database**: SQLite (single-file, embedded database)
- **Authentication**: shinymanager package
- **Data Visualization**: ggplot2, plotly
- **Data Processing**: dplyr, tidyverse
- **Export**: writexl (Excel file generation)

### Application Structure

```
expense-tracker-acted/
├── app.R                      # Main application entry point
├── R/
│   ├── core/
│   │   ├── config.R           # Configuration and dependencies
│   │   ├── db.R               # Database connection and queries
│   │   ├── contracts.R        # Data validation and business rules
│   │   └── components.R       # Reusable UI components
│   └── features/
│       ├── requests_page.R    # Main UI navigation
│       ├── requests_service.R # Request management logic
│       ├── requests_modals.R  # Request forms and modals
│       ├── finance_page.R     # Financial updates UI
│       ├── finance_service.R  # Financial processing logic
│       ├── fx_service.R       # Currency exchange rate service
│       ├── manage_service.R   # Management tools logic
│       └── insights_service.R # Analytics and reporting
├── db/migrations/             # Database migration scripts
├── sql/schema.sql             # Database schema definition
├── www/acted-logo.png         # Static assets
├── docs/                      # Documentation
└── tracker.db                 # SQLite database file
```

---

## 🚀 Installation

### Prerequisites

- **R** (version 4.0 or higher)
- **RStudio** (recommended for development)

### Required R Packages

```r
install.packages(c(
  "shiny",
  "bslib",
  "DBI",
  "RSQLite",
  "DT",
  "dplyr",
  "writexl",
  "ggplot2",
  "scales",
  "shinymanager"
))
```

### Quick Start

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd expense-tracker-acted
   ```

2. **Install dependencies**
   ```r
   # Open R or RStudio in the project directory
   source("R/core/config.R")  # Checks and loads all required packages
   ```

3. **Run the application**
   ```r
   shiny::runApp()
   ```

4. **Access the application**
   - Open browser to `http://127.0.0.1:3607` (or the displayed port)
   - Login with credentials: **username:** `acted`, **password:** `logapp`

---

## ⚙️ Configuration

### Authentication Credentials

Default credentials are configured in `R/core/config.R`:

```r
AUTH_CREDENTIALS <- data.frame(
  user = "acted",
  password = "logapp",
  stringsAsFactors = FALSE
)
```

**⚠️ SECURITY WARNING**: Change these credentials in production environments!

### Currency Settings

Default currency and available currencies are configured in `R/core/config.R`:

```r
CUR <- c("ETB", "USD", "EUR", "GBP")
DEFAULTS$currency <- "ETB"
```

---

## 📖 Usage

### Login
- Navigate to the application URL
- Enter **username:** `acted`
- Enter **password:** `logapp`
- Click "Log In"

### Create a New Request
1. Go to the **Requests** tab
2. Click **"+ New Request"**
3. Fill in required fields (budget code, budget line, description, amount, etc.)
4. Click **"Submit Request"**

### Review and Approve
1. Filter requests by status: "Review"
2. Select a request from the table
3. Click to open request details
4. Click **"Approve"** to move to Pending status

### Mark as Paid
1. Filter requests by status: "Pending"
2. Select an approved request
3. Click **"Mark as Paid"**
4. Enter payment date and reference
5. Confirm payment

### View Insights
1. Go to **Insights** tab
2. Select reporting period and chart type
3. Apply filters for detailed analysis
4. Export filtered data if needed

### Logout
Click the **red logout button** (arrow icon) in the top-right navigation bar to end your session securely.

---

## 🔐 Authentication

### Default Credentials

```
Username: acted
Password: logapp
```

### Changing Credentials

Edit `R/core/config.R`:

```r
AUTH_CREDENTIALS <- data.frame(
  user = "your_username",
  password = "your_secure_password",
  stringsAsFactors = FALSE
)
```

### Security Best Practices

For production deployments:
1. Use strong, unique passwords
2. Store credentials in environment variables
3. Use HTTPS for all connections
4. Implement session timeouts
5. Regular security audits

---

## 🗄️ Database

### SQLite (Default)

The application uses SQLite for local/development deployments:

- **Database file**: `tracker.db` (in project root)
- **Advantages**: No server setup, single-file, perfect for development
- **Backup**: Simply copy the `tracker.db` file

### Backup

```bash
# SQLite backup
cp tracker.db tracker_backup_$(date +%Y%m%d).db
```

---

## 📁 Project Structure

### Core Modules

- **`app.R`**: Application entry point, authentication wrapper, server initialization
- **`R/core/config.R`**: Package loading, database path, authentication credentials
- **`R/core/db.R`**: Database connection management, query functions
- **`R/core/contracts.R`**: Data validation, business rules
- **`R/core/components.R`**: Reusable UI components

### Feature Modules

- **`R/features/requests_page.R`**: Main navigation UI structure
- **`R/features/requests_service.R`**: Request CRUD operations
- **`R/features/requests_modals.R`**: Forms and modals
- **`R/features/finance_page.R`**: Financial updates UI
- **`R/features/finance_service.R`**: Batch processing
- **`R/features/fx_service.R`**: Exchange rate service
- **`R/features/manage_service.R`**: Management tools
- **`R/features/insights_service.R`**: Analytics and reporting

---

## 🛠️ Development

### Local Development Setup

1. Clone repository
2. Install R packages
3. Open `expense-tracker-acted.Rproj` in RStudio
4. Run `shiny::runApp()` or click "Run App" button

### Code Style

- Follow tidyverse style guide
- Use consistent indentation (2 spaces)
- Comment complex business logic
- Keep functions focused and modular

---

## 🚀 Deployment

### Local Deployment

```r
# Run the app
shiny::runApp()
```

### Shiny Server

1. Install Shiny Server on your server
2. Copy project to `/srv/shiny-server/expense-tracker/`
3. Configure authentication and database connection
4. Access via `http://your-server/expense-tracker/`

### Security Considerations for Production

1. **Change default credentials**
2. **Use HTTPS** (configure SSL certificates)
3. **Regular database backups**
4. **Restrict file permissions**
5. **Use environment variables** for sensitive config
6. **Keep R packages updated**

---

## 📚 Documentation

### Available Documentation

- **[Product Specification](docs/PRODUCT_SPECIFICATION.md)**: Complete product requirements
- **[Database Design](docs/DATABASE_DESIGN.md)**: Schema and relationships
- **[File Structure](docs/FILE_STRUCTURE.md)**: Code organization
- **[Agent Instructions](AGENTS.md)**: AI-assisted development guidelines

---

## 📄 License

This project is proprietary software developed for ACTED. All rights reserved.

---

## 👥 Support

For support, questions, or feature requests:
- **Internal**: Contact ACTED IT Support
- **Technical Issues**: Open an issue in the repository

---

## 🙏 Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)
- UI components powered by [bslib](https://rstudio.github.io/bslib/)
- Developed for ACTED Logistics Operations

---

<div align="center">

**Made with ❤️ for ACTED**

</div>