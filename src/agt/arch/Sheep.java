package arch;

import jason.architecture.AgArch;

public class Sheep extends AgArch {
    @Override
    public void init() throws Exception {
        super.init();

        System.out.println("hiiiiiiii????????????");
        System.out.println(getAgName());
        System.out.println(getTS().getAg());
        System.out.println(getTS().getAgArch());
        System.out.println(getTS().getAgArch().getAgName());
    }
}
