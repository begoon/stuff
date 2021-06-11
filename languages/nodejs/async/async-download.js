const downloadFile = async file => {
    const result = Math.random() < 0.3;
    console.log(`downloading ${file} ${result ? 'successful' : 'failed'}`)
    if (!result) {
        setImmediate(() => downloadFile(file));
    }
    return result;
}

const downloadFiles = async (files) => {
    files.forEach(file => downloadFile(file));
}

(async () => downloadFiles(['a', 'b', 'c', 'd']))();
