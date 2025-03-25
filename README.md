# OCaml Stock Trading Simulator

A robust portfolio management system built in OCaml that allows users to simulate stock trading with both historical data and real-time market data.

## Features

- **Dual Mode Operation**:
  - Simulated mode with historical stock data
  - Real-time mode with live market prices via API integration

- **Portfolio Management**:
  - Buy and sell stocks with immediate execution
  - View current holdings and portfolio valuation
  - Track cash balance and investment performance

- **Data Handling**:
  - Load historical stock data from CSV files
  - Fetch real-time stock prices through async API calls
  - Simulate market changes in historical mode with pattern-based price updates

- **Persistence**:
  - Save and load portfolio state across sessions
  - Transaction history tracking
  - Multiple portfolio file support

- **User Experience**:
  - Color-coded terminal interface with ASCII art
  - Interactive menu system
  - Comprehensive help documentation
  - Confirmation dialogs for important actions

## Technical Architecture

### Core Components

#### API Interface
- `Api` module for asynchronous stock price fetching
- Uses Lwt for non-blocking network operations
- Error handling for failed API requests

#### Portfolio Management
- `Portfolio` module for historical simulation
- `Rt_portfolio` module for real-time trading
- Abstract data types to protect portfolio integrity
- Transaction validation and execution

#### Stock Data Handling
- `Stock` module for stock price representation
- CSV parsing for historical data
- Pattern-based price simulation ("high", "medium", "low")

#### Persistence Layer
- `Save_portfolio` and `Rt_save_portfolio` modules
- File-based storage in CSV/JSON format
- Automatic portfolio recovery on startup

## Technical Highlights

- **Asynchronous Programming**: Uses OCaml's Lwt library for non-blocking I/O operations
- **Functional Design**: Immutable data structures and pure functions for predictable behavior
- **Type Safety**: Leverages OCaml's strong type system to prevent runtime errors
- **Error Handling**: Comprehensive error handling with option types
- **Terminal UI**: Rich text interface using ANSITerminal for colors and formatting
- **Random Market Simulation**: Pattern-based price evolution algorithms

## Usage

### Installation

```bash
# Install dependencies
opam install lwt yojson ANSITerminal csv unix

# Build the project
dune build

# Run the application
dune exec bin/main.exe
```

### Getting Started

1. Choose between simulated or real-time portfolio mode
2. Initialize with starting balance
3. Navigate the menu to buy/sell stocks and track performance
4. Save portfolio progress before exiting

## Menu System

### Simulated Mode
- Buy stock - Purchase shares using historical data
- Sell stock - Liquidate shares from your portfolio
- View portfolio - See your current holdings and value
- Exit to earnings call - Update stock prices based on patterns
- Save portfolio - Store your portfolio state
- Help - Display command options

### Real-Time Mode
- Buy stock - Purchase shares at current market prices
- Sell stock - Sell shares at current market prices
- View portfolio - See real-time portfolio valuation
- Save portfolio - Store your portfolio state
- Help - Display command options


### Key Modules
- **Api**: Interface for stock price API requests
- **Portfolio**: Core portfolio state management
- **Stock**: Stock data structures and operations
- **Save_portfolio**: Portfolio persistence logic


## Dependencies

- **Core OCaml Libraries**: Lwt, Unix, Printf
- **External Libraries**: ANSITerminal for formatting
- **Data Format Libraries**: Yojson, CSV











Authors : 
Nick Spoto (njs232)
Tamer Gabal (tg432)
Yash Chatha (yc2727)
Farhan Mashrur (fm454)
Ahmed Abdulla (aaa384)
