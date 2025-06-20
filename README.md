# Token Rewards for Volunteer Work

A decentralized platform that rewards volunteers with proof tokens for verified service time and impact.

## 🚀 Features

- Register organizations and volunteers
- Log and verify volunteer hours
- Earn proof tokens for verified activities 
- Track volunteer statistics and impact
- Organization verification system

## 🛠️ Technical Details

### Security Features
- Owner-only access control for critical functions
- Input validation for hours and activity data
- Organization verification requirements
- Double-verification prevention

### Optimizations
- Efficient data structures using maps
- Minimal state changes
- Batched updates for volunteer statistics

## 📋 Usage Instructions

1. Deploy contract using Clarinet
2. Organizations register using `register-organization`
3. Contract owner verifies organizations
4. Volunteers register using `register-volunteer`
5. Log hours with `log-volunteer-hours`
6. Organizations verify hours with `verify-volunteer-hours`

## 🧪 Testing

Run tests using:
```bash
clarinet test
```

## 💻 UI Components

Suggested UI features:
- Dashboard showing volunteer statistics
- Activity submission form
- Organization verification portal
- Impact visualization with charts
- Profile pages for volunteers and organizations

## 🔄 Git Details
