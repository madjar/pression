import QtQuick 2.5
import QtQuick.Controls 1.4

ApplicationWindow {
    title: qsTr("Pression")
    visible: true
    flags: "Dialog"

    Column {
        anchors.fill: parent
        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: game
        }

        Row {
            spacing: 20

            Button {
                text: qsTr("Launch")
                onClicked: launch()
            }

            Button {
                text: qsTr("Another")
                onClicked: changeGame()
            }
        }
    }
}
