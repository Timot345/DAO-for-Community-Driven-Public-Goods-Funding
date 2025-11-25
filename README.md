# 🏛️ DAO for Community-Driven Public Goods Funding

A decentralized autonomous organization (DAO) smart contract that enables community members to propose, vote on, and fund local public goods projects using Bitcoin-backed funding.

## 🚀 Features

- 👥 **Member Management**: Join/leave the DAO
- 💰 **Treasury Management**: Deposit STX funds for project funding
- 📝 **Proposal Creation**: Submit funding proposals for community projects
- 🗳️ **Democratic Voting**: Vote on proposals with transparent tallying
- ⚡ **Automated Execution**: Execute approved proposals automatically
- 🔒 **Security**: Owner controls for emergency situations

## 🏗️ How It Works

1. **Join the DAO** 🤝 - Become a member to participate
2. **Fund the Treasury** 💵 - Deposit STX to enable project funding
3. **Create Proposals** 📋 - Submit ideas for community projects (parks, libraries, etc.)
4. **Vote** 🗳️ - Members vote for or against proposals
5. **Execute** ✅ - Approved proposals automatically receive funding

## 📋 Usage Instructions

### Join the DAO
```clarity
(contract-call? .dao-community-goods-funding join-dao)
```

### Deposit Funds to Treasury
```clarity
(contract-call? .dao-community-goods-funding deposit-funds u1000000) ;; 1 STX
```

### Create a Proposal
```clarity
(contract-call? .dao-community-goods-funding create-proposal 
  "Community Park Renovation"
  "Renovate the local park with new playground equipment and walking paths"
  u500000 ;; 0.5 STX
  'SP1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE) ;; recipient address
```

### Vote on a Proposal
```clarity
(contract-call? .dao-community-goods-funding vote u1 true) ;; Vote FOR proposal #1
(contract-call? .dao-community-goods-funding vote u1 false) ;; Vote AGAINST proposal #1
```

### Execute an Approved Proposal
```clarity
(contract-call? .dao-community-goods-funding execute-proposal u1)
```

## 🔍 Read-Only Functions

### Check Proposal Details
```clarity
(contract-call? .dao-community-goods-funding get-proposal u1)
```

### Check if Member
```clarity
(contract-call? .dao-community-goods-funding is-member 'SP1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE)
```

### Check Treasury Balance
```clarity
(contract-call? .dao-community-goods-funding get-treasury-balance)
```

### Check Proposal Status
```clarity
(contract-call? .dao-community-goods-funding get-proposal-status u1)
```

## ⚙️ Configuration

- **Voting Period**: 1440 blocks (~10 days)
- **Minimum Votes Required**: 3 votes minimum
- **Owner Controls**: Emergency withdrawal and parameter updates

## 🔧 Development

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks CLI for deployment

### Testing
```bash
clarinet test
```

### Check Contract
```bash
clarinet check
```

### Deploy
```bash
clarinet deploy
```

## 🏗️ Project Structure

```
├── contracts/
│   └── Dao-Community-Goods-Funding.clar
├── tests/
├── settings/
├── Clarinet.toml
└── README.md
```

## 💡 Example Use Cases

- 🌳 **Park Improvements**: Fund new benches, trees, or playground equipment
- 📚 **Library Resources**: Purchase books, computers, or educational materials
- 🚦 **Infrastructure**: Street lighting, crosswalks, or bike lanes
- 🎨 **Community Art**: Murals, sculptures, or cultural installations
- 🏥 **Health Initiatives**: Community gardens or fitness equipment

## 🛡️ Security Features

- Member-only proposal creation and voting
- Minimum vote thresholds for proposal execution
- Time-locked voting periods
- Owner emergency controls
- Automatic fund transfers only to approved recipients

## 📄 License

This project is open source and available under the MIT License.

---

Built with ❤️ for community empowerment and decentralized governance.
