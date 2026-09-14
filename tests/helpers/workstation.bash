workstation_vet_fixture() {
    @ WorkstationSchema validate: "$(cat "$1")" as: "$2"
}
