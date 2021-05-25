const path = require('path')
const express = require('express')

const app = express()
const publicDirectoryPath = path.join(__dirname, '../public')

app.set('view engine', 'hbs')
app.use(express.static(publicDirectoryPath))

app.get('', (req, res) => {
    res.render('index', {
        title: 'Title',
        name: 'Name'
    })
})

app.get('/api', (req, res) => {
    res.send({
        title: 'Title',
        name: 'Name'
    })
})

app.listen(3000, () => {
    console.log('Server is up on port 3000.')
})
