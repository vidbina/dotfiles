function layout() {
    return {
        name: "3 Frames",
        getFrameAssignments: (windows, screenFrame) => {
            const halfWidth = Math.round(screenFrame.width / 2)
            const halfHeight = Math.round(screenFrame.height / 2)

            function calculateFrameForIndex(index, length) {
                switch(index) {
                    case 0:
                        return {
                            ...screenFrame,
                            // At multiple windows, half window width into 2 panes
                            width: length > 1 ? halfWidth : screenFrame.width
                        };
                    case 1:
                        return {
                            ...screenFrame,
                            // Always position window 2 (index 1) in 2nd pane
                            x: screenFrame.x + halfWidth,
                            width: halfWidth,
                            // For 3 or more windows, half window height into 2 panes
                            height: length > 2 ? halfHeight : screenFrame.height,
                        };
                    default:
                        return {
                            x: screenFrame.x + halfWidth,
                            y: screenFrame.y + halfHeight,
                            width: halfWidth,
                            height: halfHeight,
                        };
                }
            }

            return windows.reduce((frames, window, index) => {
                return {
                    ...frames,
                    [window.id]: calculateFrameForIndex(index, windows.length)
                };
            }, {});
        }
    };
}