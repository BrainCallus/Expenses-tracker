# Expenses tracker

The app was developed as an educational project for the Tinkoff Scala course primarily for entertainment purposes, so the author admits the presence of bugs and imperfections in the project, although attempted to make the application qualitatively and provide a user-friendly interface 😽

**Currently, the interface maintains:**
- new user registration
- app usage with an existing account
- adding or deleting expenses for past dates
- scheduling expenses for the following dates
- confirming or rejecting previously scheduled payments
- forecasting total expenses at the end of the current month based on previous statistics

### What needs to be done to start
1. In the `docker-compose.yaml` and the `/app/model/dao/io/DbIOProvider.scala` set the default db login and password. If you want, change them to yours
2. In the root directory run `build.bat` in cmd to create containers for db, adminer(helpful to manage db) and python server
3. For clarity there is in the `/app/model/dao/DBUtils.scala` a code that creates in db several users and their expenses. If you don't want to sand many forms by hand and meanwhile want to see functionality, run it
4. Run in the root directory `docker-start.bat` that starts created containers
5. Open expenses-tracker in, for ex idea, and run it as a Play2Run configuration or by cmd with `sbt run` 
6. For tests run `sbt test`