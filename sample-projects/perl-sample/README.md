# Perl Sample Project

A sample Perl project for testing lem's perl-mode.

## Structure

```
perl-sample/
├── bin/
│   └── app.pl          # Main application
├── lib/
│   └── MyApp/
│       ├── Utils.pm    # Utility functions
│       └── Calculator.pm # Calculator class
├── t/
│   ├── 01_utils.t      # Utils tests
│   └── 02_calculator.t # Calculator tests
├── cpanfile            # Dependencies
└── README.md
```

## Running

```bash
# Run the main application
perl bin/app.pl

# Run tests
prove -l t/
```

## Features Demonstrated

- Package/module definition
- Exporter usage
- Object-oriented Perl (blessed references)
- POD documentation
- Test::More testing
- Error handling with Carp
