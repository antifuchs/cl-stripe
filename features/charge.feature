Feature: Charges

  As a user of stripe,
  I want to create charges
  so I can get paid by my customers.

Background:

  When I use the API key "wOUwrSt8JrWsPyHCsfkCDhCCqGomv4kp"

Scenario: Create a charge with a credit card
  
  When I charge $20 to a random credit card

  Then the charge should exist
  And the charge should be for $20

Scenario: Create a charge with a token

  When I create a token for a random credit card
  And I charge ct3033 to the token

  Then the charge should exist
  And the charge should be for ct3033
