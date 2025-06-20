import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';

Clarinet.test({
  name: "Ensures organization registration works",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    let block = chain.mineBlock([
      Tx.contractCall(
        "Token-Rewards-for-Volunteer-Work",
        "register-organization",
        [types.ascii("Test Org")],
        deployer.address
      ),
    ]);
    block.receipts[0].result.expectOk().expectBool(true);
  },
});

Clarinet.test({
  name: "Ensures volunteer registration works",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const volunteer = accounts.get("wallet_1")!;
    let block = chain.mineBlock([
      Tx.contractCall(
        "Token-Rewards-for-Volunteer-Work",
        "register-volunteer",
        [],
        volunteer.address
      ),
    ]);
    block.receipts[0].result.expectOk().expectBool(true);
  },
});

Clarinet.test({
  name: "Ensures logging hours works",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const org = accounts.get("deployer")!;
    const volunteer = accounts.get("wallet_1")!;

    let block = chain.mineBlock([
      Tx.contractCall(
        "Token-Rewards-for-Volunteer-Work",
        "register-organization",
        [types.ascii("Test Org")],
        org.address
      ),
      Tx.contractCall(
        "Token-Rewards-for-Volunteer-Work",
        "register-volunteer",
        [],
        volunteer.address
      ),
      Tx.contractCall(
        "Token-Rewards-for-Volunteer-Work",
        "log-volunteer-hours",
        [types.principal(org.address), types.uint(10), types.ascii("Test activity")],
        volunteer.address
      ),
    ]);
    block.receipts[2].result.expectOk();
  },
});
