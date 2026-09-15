workstation_vet_fixture() {
    @ Workstation::Schema validate: "$(cat "$1")" as: "$2"
}
